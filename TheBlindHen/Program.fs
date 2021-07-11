open System
open Model
open ExtCore.Args

// Play with command line arguments
let problemPath = ref None
let problemNo = ref None
let gui = ref false
let argSpecs =
    [ "-pp", ArgType.String (fun p -> problemPath := Some p), "Path to problems"
    ; "-p", ArgType.Int (fun p -> problemNo := Some p), "Problem number"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

[<EntryPoint>]
let main args =
    ArgParser.Parse(argSpecs, fun s -> failwith $"Unknown argument: {s}")
    match !problemPath, !problemNo with
    | Some problemPath, Some problemNo ->
        let problem = Model.parseFile $"{problemPath}/{problemNo}.problem"
        printfn "%A" problem
        let solutionDir = $"{problemPath}/{problemNo}-solutions/"
        if !gui then
            GUI.showGui problem solutionDir
        else
            FitInHole.solve problem solutionDir
            0
    | _ ->
        printfn "Must specify both problem path and problem"
        1