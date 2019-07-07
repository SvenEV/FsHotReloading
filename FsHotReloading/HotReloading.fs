namespace FsHotReloading

open System
open System.IO
open System.Diagnostics
open System.Collections.Generic

type RegistryId = RegistryId of string

[<StructuredFormatDisplay("{AsString}")>]
type ValueId =
    | ValueId of RegistryId * obj
    member id.AsString =
        match id with ValueId ((RegistryId regId), key) -> sprintf "%s::%A" regId key

module ValueId =
    let unbox (ValueId (_, id)) = unbox id
    let registryId (ValueId (regId, _)) = regId
    let box registryId id = ValueId (registryId, id)

type Change =
    | ValueCreated
    | ValueChanged
    | ValueDestroyed
    
type RegistryOps = {
    registryId: RegistryId
    getFileFromKey: ValueId -> string
    tryFind: ValueId -> (obj * obj) option
    getAllKeys: unit -> ValueId seq
    allocate: ValueId -> obj -> obj -> unit
    update: ValueId -> obj -> unit
    deallocate: ValueId -> unit
    onChange: Change -> ValueId -> unit
}

type RegistryInterface<'key, 'state, 'result> = {
    ops: RegistryOps
    register: 'key -> 'state -> 'result -> unit
    tryGet: 'key -> 'state option
}

module private HotReloadSystem =
    
    type HotReloadSystemState = {
        watchers: Map<string, FileSystemWatcher>
    }
    
    type ValueUpdateResult =
        | ValueUpdated of id: ValueId
        | ValueNewlyRegistered of id: ValueId * state: HotReloadSystemState
        | ValueDestroyed of id: ValueId
        | ValueUpdateFailedWrongType of id: ValueId * loadedType: Type 
    
    type Context = 
        | NormalContext
        | FsiContext of update: (ValueId -> obj -> obj -> unit)

    type RegisterValueResult =
        | ValueRegistered
        | ValueAlreadyRegisteredError

    type ReloadFileResult =
        | FileReloaded of ValueUpdateResult list
        | ScriptEvaluationError of Fsi.EvalError

    type ProcessMessageResult =
        | ReloadFileResult of filePath: string * ReloadFileResult
        | UnloadFileResult of filePath: string * destroyed: ValueId list
        | RegisterValueResult of ValueId * RegisterValueResult
        | ReadValueResult of ValueId

    type Message =
        | ReloadFile of file: string * triggerUpdates: AsyncReplyChannel<(Change * ValueId) list>
        | UnloadFile of file: string * triggerUpdates: AsyncReplyChannel<(Change * ValueId) list>
        | RegisterValue of ops: RegistryOps * id: ValueId * state: obj * result: obj
        | ReadValue of ops: RegistryOps * id: ValueId * replyChannel: ((obj * obj) option) AsyncReplyChannel

    let mutable context = NormalContext

    let triggerUpdates registries changes =
        changes
        |> List.iter (fun (change, id) ->
            let registry = registries |> Map.find (ValueId.registryId id)
            registry.onChange change id)

    let ensureWatched directoryPath registries postMessage state =
        match state.watchers.TryFind directoryPath with
        | Some _ -> state
        | None ->
            let watcher = new FileSystemWatcher(Filter = "*.fs", Path = directoryPath)
            watcher.Changed.Add (fun f -> postMessage (fun chan -> ReloadFile (f.FullPath, chan)) |> triggerUpdates registries)
            watcher.Deleted.Add (fun f -> postMessage (fun chan -> UnloadFile (f.FullPath, chan)) |> triggerUpdates registries)
            watcher.Renamed.Add (fun f -> postMessage (fun chan -> UnloadFile (f.OldFullPath, chan)) |> triggerUpdates registries)
            watcher.EnableRaisingEvents <- true
            { state with watchers = state.watchers |> Map.add directoryPath watcher }

    let registerValue ops id value result registries postMessage state =
        match ops.tryFind id with
        | Some _ -> state, ValueAlreadyRegisteredError
        | None ->
            let dir = Path.GetDirectoryName (ops.getFileFromKey id)
            let state' = ensureWatched dir registries postMessage state
            ops.allocate id value result
            state', ValueRegistered

    let updateValue ops state id value result registries postMessage =
        match ops.tryFind id with
        | None ->
            // Note: 'ValueAlreadyRegisteredError' can't occur here
            let (state', _) = registerValue ops id value result registries postMessage state
            ValueNewlyRegistered (id, state')
        | Some _ ->
            try
                ops.update id value
                ValueUpdated id
            with :? InvalidCastException ->
                ValueUpdateFailedWrongType (id, value.GetType())

    let unloadFile registries filePath =
        registries
        |> Map.values
        |> Seq.collect (fun ops ->
            ops.getAllKeys()
            |> Seq.filter (fun id -> ops.getFileFromKey id = filePath)
            |> Seq.map (fun id -> ops.deallocate id; id))
        |> List.ofSeq

    let reloadFile registries file fsi postMessage state =
        let updates = Dictionary()
        let mutable state = state
        
        let update id value result =
            let ops = registries |> Map.find (ValueId.registryId id)
            let result = updateValue ops state id value result registries postMessage

            let state' =
                match result with
                | ValueUpdated _ -> state
                | ValueNewlyRegistered (_, state') -> state'
                | ValueDestroyed _ -> failwith "ValueDestroyed can't happen here"
                | ValueUpdateFailedWrongType _ -> state
            
            state <- state'
            Dictionary.add id result updates

        context <- FsiContext update

        match Fsi.evalScript file fsi with
        | Error error ->
            context <- NormalContext
            state, ScriptEvaluationError error
        | Ok () ->
            // Destroy stale values
            registries
            |> Map.values
            |> Seq.collect (fun ops ->
                ops.getAllKeys()
                |> Seq.filter (fun id -> ops.getFileFromKey id = file && not (updates.ContainsKey id))
                |> Seq.map (fun id -> ops, id))
            |> Seq.iter (fun (ops, id) ->
                ops.deallocate id
                Dictionary.add id (ValueDestroyed id) updates)

            // Report results
            let results = List.ofSeq updates.Values
            context <- NormalContext
            state, FileReloaded results

    let processMessage state registries fsi postMessage = function
        | ReloadFile (file, chan) ->
            let state', result = state |> reloadFile registries file fsi postMessage
            match result with
            | ScriptEvaluationError _ -> chan.Reply []
            | FileReloaded results ->
                results
                |> List.choose (function
                    | ValueUpdated id -> Some (ValueChanged, id)
                    | ValueNewlyRegistered (id, _) -> Some (ValueCreated, id)
                    | ValueDestroyed id -> Some (Change.ValueDestroyed, id)
                    | ValueUpdateFailedWrongType _ -> None)
                |> chan.Reply
            state', ReloadFileResult (file, result)
        | UnloadFile (file, chan) ->
            let destroyed = unloadFile registries file
            destroyed
            |> List.map (fun id -> Change.ValueDestroyed, id)
            |> chan.Reply
            state, UnloadFileResult (file, destroyed)
        | RegisterValue (ops, id, value, result) ->
            let (state', result) = state |> registerValue ops id value result registries postMessage
            state', RegisterValueResult (id, result)
        | ReadValue (ops, id, replyChannel) ->
            replyChannel.Reply <| ops.tryFind id
            state, ReadValueResult id

[<RequireQualifiedAccess>]
module HotReloading =
    open HotReloadSystem

    let private updateResultToLogMessage = function
        | ValueUpdated id ->
            sprintf "'%A': Updated" id
        | ValueNewlyRegistered (id, _) ->
            sprintf "'%A': Registered" id
        | ValueDestroyed id ->
            sprintf "'%A': Destroyed" id
        | ValueUpdateFailedWrongType (id, loadedType) ->
            sprintf "'%A': Error, type changed from '%s' to '%s'" id "(original type)" loadedType.Name

    let private resultToLogMessage = function
        | ReloadFileResult (filePath, result) ->
            let file = Path.GetFileName filePath
            match result with
            | FileReloaded results ->
                let perValueResults =
                    results
                    |> List.map updateResultToLogMessage
                    |> String.joinn
                    |> String.indent 4
                Some (sprintf "Reloaded '%s':\n%s" file perValueResults)
            | ScriptEvaluationError error ->
                Some (sprintf "Failed to evaluate script '%s': %A" file error)
        | UnloadFileResult (filePath, ids) ->
            let file = Path.GetFileName filePath
            let perValueResults =
                ids
                |> List.map (ValueDestroyed >> updateResultToLogMessage)
                |> String.joinn
                |> String.indent 4
            Some (sprintf "Unloaded '%s':\n%s" file perValueResults)
        | RegisterValueResult (id, result) ->
            match result with
            | ValueRegistered -> Some (sprintf "Registered value '%A'" id)
            | ValueAlreadyRegisteredError -> Some (sprintf "Attempted to re-register '%A'" id)
        | ReadValueResult id -> None // sprintf "Read value '%A'" id

    let private agent registries (mailbox: Message MailboxProcessor) = async {
        let log msg = Debug.WriteLine ("Hot reload agent: " + msg)
        let registries =
            registries
            |> Seq.map (fun ops -> ops.registryId, ops)
            |> Map.ofSeq

        match Fsi.determineFsiPath() with
        | None -> log "Failed to determine 'fsi.exe' path"
        | Some fsiPath ->
            let fsi = Fsi.createSession fsiPath
            let mutable state = { watchers = Map.empty }
            log "Started"

            while true do
                let! msg = mailbox.Receive()
                let state', result = processMessage state registries fsi mailbox.PostAndReply msg
                state <- state'
                resultToLogMessage result |> Option.iter log
    }

    let mutable private mailbox = None
    
    let enable registries =
        match mailbox with
        | None -> mailbox <- MailboxProcessor.Start (agent registries) |> Some
        | Some _ -> invalidOp "Hot reloading is already enabled"

    let private registerValue ops (id: obj) (value: obj) (result: obj) =
        let id = ValueId.box ops.registryId id
        match mailbox, context with
        | Some mailbox, NormalContext ->
            mailbox.Post (RegisterValue (ops, id, value, result))
        | Some _, FsiContext update ->
            update id value result
        | None, _ ->
            ops.allocate id value result

    let private getValue ops id =
        let id = ValueId.box ops.registryId id
        match mailbox, context with
        | Some mailbox, NormalContext -> mailbox.PostAndReply (fun chan -> ReadValue (ops, id, chan))
        | Some _, FsiContext _ -> ops.tryFind id
        | None, _ -> ops.tryFind id
        |> Option.map fst
    
    let createRegistry<'key, 'state, 'result> registryId
        (getFileFromKey: 'key -> string)
        (tryFind: 'key -> ('state * 'result) option)
        (getAllKeys: unit -> 'key seq)
        (allocate: 'key -> 'state -> 'result -> unit)
        (update: 'key -> 'state -> unit)
        (deallocate: 'key -> unit)
        (onChange: Change -> 'key -> unit) =
        
        let ops =
            {
                registryId = registryId
                getFileFromKey = ValueId.unbox >> getFileFromKey
                tryFind = ValueId.unbox >> tryFind >> Option.map (fun (state, result) -> unbox state, unbox result)
                getAllKeys = getAllKeys >> Seq.map (ValueId.box registryId)
                allocate = fun key state result -> allocate (ValueId.unbox key) (unbox state) (unbox result)
                update = fun key state -> update (ValueId.unbox key) (unbox state)
                deallocate = ValueId.unbox >> deallocate
                onChange = fun change id -> onChange change (ValueId.unbox id)
            }

        let register (key: 'key) (state: 'state) (result: 'result) =
            registerValue ops key state result

        let get id =
            getValue ops id |> Option.map unbox

        {
            ops = ops
            register = register
            tryGet = get
        }

    let createSimpleRegistry name getFileFromKey =
        let values = Dictionary()
        let changeEvent = Event<Change * 'key>()

        let interf =
            createRegistry (RegistryId name)
            <| getFileFromKey
            <| values.TryFind
            <| fun () -> seq values.Keys
            <| (fun id state result -> Dictionary.add id (state, result) values)
            <| (fun id state -> Dictionary.add id (state, snd values.[id]) values)
            <| (fun id -> Dictionary.remove id values |> ignore)
            <| (fun change id -> changeEvent.Trigger (change, id))
        
        interf, changeEvent.Publish