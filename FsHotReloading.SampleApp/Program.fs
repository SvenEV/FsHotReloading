module FsHotReloading.SampleApp.Program

open FsHotReloading
open System

[<EntryPoint>]
let main _ =
    HotReloading.enable [ StringRegistry.registry.ops ]

    let renderText _ =
        Console.Clear()
        printfn "Hello, %s! It's %s today!" (KnownStrings.myName()) (KnownStrings.weather())

    StringRegistry.onChange.Subscribe renderText |> ignore
    renderText()

    Console.Read() |> ignore
    0
