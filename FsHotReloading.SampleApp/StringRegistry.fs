namespace FsHotReloading.SampleApp

open System.Runtime.CompilerServices
open FsHotReloading

type StringId = {
    file: string
    name: string
}

module StringRegistry =
    let registry, onChange = HotReloading.createSimpleRegistry "HotReloadableStrings" (fun id -> id.file)

type StringRegistry =

    static member register (value: string, [<CallerMemberName>]?name: string, [<CallerFilePath>]?file: string) =
        let id = { file = file.Value; name = name.Value }
        let getter = fun () -> StringRegistry.registry.tryGet id |> Option.get
        StringRegistry.registry.register id value getter
        getter