open System
open Model
open ExtCore.Args

// Play with command line arguments
let problemPath = ref None
let problemNo = ref None
let gui = ref false
let writeToFile = ref false
let seed = ref (int DateTime.Now.Ticks)
let solution = ref None

let argSpecs =
    [ "-pp", ArgType.String (fun p -> problemPath := Some p), "Path to problems"
    ; "-p", ArgType.Int (fun p -> problemNo := Some p), "Problem number"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ; "-w", ArgType.Unit (fun () -> writeToFile := true), "Write solution to file"
    ; "-seed", ArgType.Int (fun s -> seed := s), "Randomness seed to use"
    ; "-sol", ArgType.String (fun sol -> solution := Some sol), "Don't run, but score the input solution to the given problem"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo.Create(sh, ty, desc))
      |> Array.ofList

// TODO: .NET documents that two processes started at the same time will get the
// same random seed here. Mix with process id?
let fileNameRandomizer = Random()

let bestCurrentSolution (dirinfo: IO.DirectoryInfo) =
    dirinfo.EnumerateFiles ()
    |> Seq.filter (fun f -> f.Name |> Seq.forall Char.IsDigit)
    |> Seq.map (fun f -> int(f.Name))
    |> Seq.sort
    |> Seq.tryHead

let writeSolution solutionDir problem figure =
    let solutionText = Model.deparseSolution(Model.solutionOfFigure(figure))
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    let dislikes = Penalty.dislikesPenalty problem figure
    let solutionFile = sprintf "%s%d" solutionDir dislikes
    // Write the file only if the directory is empty
    match bestCurrentSolution dirinfo with
    | None ->
        printfn "Found a new solution (dislikes: %d). Writing solution to %s" dislikes solutionFile
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) when dislikes < best ->
        printfn "Found a better solution (dislikes: %d -> %d). Writing solution to %s" best dislikes solutionFile
        IO.File.WriteAllText(solutionFile, solutionText)
    | Some(best) ->
        printfn "A better solution already exists (dislikes: %d). Not writing a new file. Solution (dislikes: %d):" best dislikes
        printfn "%s" solutionText

let printSolution figure =
    printfn "A solution was found, but it will not be written to a file unless -w is given."
    let solution = Model.solutionOfFigure(figure)
    printfn "Solution: %s" (Model.deparseSolution solution)

[<EntryPoint>]
let main args =
    ArgParser.Parse(argSpecs, fun s -> failwith $"Unknown argument: {s}")
    // Setup global random number generator as the first thing
    printfn "Random seed for this run: %d" !seed
    Util._rnd := Some (Random(!seed))
    match !problemPath, !problemNo with
    | Some problemPath, Some problemNo ->
        let file = $"{problemPath}/{problemNo}.problem"
        printfn "Reading problem file %s" file
        let problem = parseFile file
        let solutionDir = $"{problemPath}/{problemNo}-solutions/"
        match !solution, !gui with
        | Some solFile, _ ->
            printfn $"Scoring solution {solFile}:"
            let figure = parseSolutionFile solFile
                        |> figureOfSolution problem
            printfn $"\t{Penalty.figurePenaltiesToString problem figure}"
            printfn $"Dislikes: {Penalty.dislikesPenalty problem figure}"
            0
        | _, true ->
            GUI.showGui problem (writeSolution solutionDir problem)
        | _, false ->
            let writeIfTold =
                if !writeToFile then
                    writeSolution solutionDir problem
                else
                    printSolution
            FitInHole.solve problem writeIfTold
            0
    | _ ->
        printfn "Must specify both problem path and problem"
        1
