open System
open Model
open ExtCore.Args

// Play with command line arguments
let inputFile = ref None
let gui = ref false
let argSpecs =
    [ "-p", ArgType.String (fun p -> inputFile := Some p), "Input problem file"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

[<EntryPoint>]
let main args =
    ArgParser.Parse(argSpecs, fun s -> failwith $"Unknown argument: {s}")
    match !inputFile with
    | None ->
        printfn "No input file given"
        1
    | Some inputFile ->
        if !gui then
            GUI.showGui inputFile
        else
            0
