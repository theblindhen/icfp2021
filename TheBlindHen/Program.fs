open System
open Model
open ExtCore.Args

// Play with command line arguments
let problemPath = ref None
let problemNo = ref None
let gui = ref false
let writeToFile = ref false

let argSpecs =
    [ "-pp", ArgType.String (fun p -> problemPath := Some p), "Path to problems"
    ; "-p", ArgType.Int (fun p -> problemNo := Some p), "Problem number"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ; "-w", ArgType.Unit (fun () -> writeToFile := true), "Write solution to file"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

let writeSolution solutionDir figure =
    let postfix = FitInHole.rnd.Next(999999)
    let solutionFile = sprintf "%s%06d" solutionDir postfix
    IO.Directory.CreateDirectory solutionDir |> ignore
    printfn "Writing solution to %s" solutionFile
    IO.File.WriteAllText(solutionFile, Model.deparseSolution(Model.solutionOfFigure(figure)))

let printSolution figure =
    printfn "A solution was found, but it will not be written to a file unless -w is given."
    let solution = Model.solutionOfFigure(figure)
    printfn "Solution: %s" (Model.deparseSolution solution)

[<EntryPoint>]
let main args =
    ArgParser.Parse(argSpecs, fun s -> failwith $"Unknown argument: {s}")
    match !problemPath, !problemNo with
    | Some problemPath, Some problemNo ->
        let problem = Model.parseFile $"{problemPath}/{problemNo}.problem"
        printfn "%A" problem
        let solutionDir = $"{problemPath}/{problemNo}-solutions/"
        if !gui then
            GUI.showGui problem (writeSolution solutionDir)
        else
            let writeIfTold =
                if !writeToFile then
                    writeSolution solutionDir
                else
                    printSolution
            FitInHole.solve problem writeIfTold
            0
    | _ ->
        printfn "Must specify both problem path and problem"
        1
