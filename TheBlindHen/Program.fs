open System
open Model
open ExtCore.Args

// TODO: Purge, remember also the unittest
// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let MAXITERS = 10

let solveProblem (problem: Problem) =
    let rnd = System.Random 0
    let neighbors = Neighbors.translateRandomCoord rnd
    let cost = Penalty.penaltyEdgeLengthSqSum problem
    Hillclimber.runHillClimber neighbors cost MAXITERS problem.Figure

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
        if !gui then
            GUI.showGui [||] (* TODO: arg passing *)
        else
            0
    | Some inputFile ->
        let problem = Model.parseFile inputFile
        printfn "%A" problem
        printfn "Brilliant solution:"
        let figure = solveProblem problem
        printfn "%s" (Model.deparseSolution (solutionOfFigure figure))
        if !gui then
            failwith "Not implemented: Gui and problems!"
        0