module FsHotReloading.Fsi

open System
open System.IO
open System.Text
open System.Reflection
open System.Diagnostics
open System.Text.RegularExpressions
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.SourceCodeServices

let frameworkAssemblies = Set.ofArray [|
    "mscorlib"
    "System.Private.CoreLib"
    "System.Runtime"
    "FSharp.Core"
|]

let determineFsiPath () =
    let dotnetVersion = ProcessStartInfo("dotnet", "--version", RedirectStandardOutput = true)
    let dotnetListSdks = ProcessStartInfo("dotnet", "--list-sdks", RedirectStandardOutput = true)
    
    let invokeCmd (startInfo: ProcessStartInfo) =
        use proc = Process.Start(startInfo)
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        output

    let parseSdkLine str =
        let m = Regex.Match (str, "^([\w\.\-]+) \[(.*)\]$")
        if m.Success
        then Some (m.Groups.[1].Value, m.Groups.[2].Value)
        else None

    let version = (invokeCmd dotnetVersion).Trim('\r', '\n')

    invokeCmd dotnetListSdks
    |> String.lines
    |> Seq.choose parseSdkLine
    |> Seq.tryFind (fun (ver, _) -> ver = version)
    |> Option.map (fun (ver, path) -> Path.Combine (path, ver, "FSharp", "fsi.exe"))
    
let createSession fsiPath =
    let sbOut = StringBuilder()
    let sbErr = StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    let config = FsiEvaluationSession.GetDefaultConfiguration()
    let args = [| fsiPath |]
    let session = FsiEvaluationSession.Create(config, args, inStream, outStream, errStream)

    let referenceAssembly (asm: Assembly) =
        System.Diagnostics.Debug.WriteLine (sprintf "#r %s" <| asm.GetName().Name)
        session.EvalInteractionNonThrowing (sprintf "#r \"\"\"%s\"\"\"" asm.Location)
        |> ignore
    
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.filter (fun asm -> not asm.IsDynamic)
    |> Seq.filter (fun asm -> asm.GetName().Name |> frameworkAssemblies.Contains |> not)
    |> Seq.iter referenceAssembly

    AppDomain.CurrentDomain.AssemblyLoad.Add (fun args -> referenceAssembly args.LoadedAssembly)

    session

type EvalError =
    | FSharpError of exn * FSharpErrorInfo[]
    | NoResultError
    | InvalidTypeError of Type

let evalExpression<'a> code (fsi: FsiEvaluationSession) =
    match fsi.EvalExpressionNonThrowing code with
    | Choice2Of2 ex, errors -> Error (FSharpError (ex, errors))
    | Choice1Of2 None, _ -> Error NoResultError
    | Choice1Of2 (Some result), _ ->
        match result.ReflectionValue with
        | :? 'a as result -> Ok result
        | other -> Error (InvalidTypeError <| other.GetType())

let evalInteraction code (fsi: FsiEvaluationSession) =
    match fsi.EvalInteractionNonThrowing code with
    | Choice2Of2 ex, errors -> Error (FSharpError (ex, errors))
    | Choice1Of2 (), _ -> Ok ()

let evalScript filePath (fsi: FsiEvaluationSession) =
    match fsi.EvalScriptNonThrowing filePath with
    | Choice2Of2 ex, errors -> Error (FSharpError (ex, errors))
    | Choice1Of2 (), _ -> Ok ()