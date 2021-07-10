open System
open Model
open ExtCore.Args

// Play with command line arguments
let problemPath = ref None
let problem = ref None
let gui = ref false
let argSpecs =
    [ "-pp", ArgType.String (fun p -> problemPath := Some p), "Path to problems"
    ; "-p", ArgType.Int (fun p -> problem := Some p), "Problem number"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

[<EntryPoint>]
let main args =
    ArgParser.Parse(argSpecs, fun s -> failwith $"Unknown argument: {s}")
    match !problemPath, !problem with
    | Some problemPath, Some problem ->
        if !gui then
            GUI.showGui problemPath problem
        else
            FitInHole.solve problemPath problem
            0
    | _ ->
        printfn "Must specify both problem path and problem"
        1