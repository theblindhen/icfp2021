open System
open Model
open ExtCore.Args

let vertexMatchSequence (problem: Problem) : (int option array) seq =
    let figAdjMap = Graph.adjacentVertices (problem.Figure)
    let edgeFromVertexMap =
        problem.Figure.Edges
        |> Array.indexed
        |> Array.collect (fun (edgeId, (fromId, toId)) ->
            [| ((fromId, toId), edgeId); ((toId, fromId), edgeId) |])
        |> Map.ofArray
    let holeSegments = holeSegments problem |> Array.ofList
    let edgeLengthRanges =
        Penalty.problemEdgeLengthSqRanges problem
        |> Array.map (fun (min, max) -> sqrt(min), sqrt(max))

    let freeIncident (partialMatch: int option array) (startId: VertexId) (maxDepth: int) =
        let nextSeq visited vertexId =
            Map.find vertexId figAdjMap
            |> Seq.ofList 
            |> Seq.map (fun next ->
                let visited = Array.copy visited
                visited.[next] <- true
                (next, visited))
        let rec nextLevel i lvl =
            if i >= maxDepth then
                lvl
            else
                let nextLvl =
                    lvl
                    |> Seq.collect (fun (pathRev, visited) ->
                        match pathRev with
                        | [] -> failwith "err"
                        | last::_ ->
                            nextSeq visited last
                            |> Seq.map (fun (next, visited) ->
                                (next::pathRev, visited)))
                Seq.concat [lvl; nextLevel (i+1) nextLvl]
        let visited = Array.init partialMatch.Length (fun _ -> false)
        visited.[startId] <- true
        let firstLvl =
            nextSeq visited startId
            |> Seq.map (fun (next, visited) ->
                ([next], visited))
        nextLevel 1 firstLvl
        |> Seq.filter(fun (path, visited) ->
            match path with
            | [] -> failwith "err"
            | last::_ -> Option.isNone partialMatch.[last]
            )
        |> Seq.map (fst >> List.rev)

    let nextCandidates (partialMatch: int option array) (prevFigId: VertexId) (holeSegLen: float) =
        freeIncident partialMatch prevFigId 2 // TODO: maxDepth
        |> Seq.filter (fun path ->
            match path with
            | [] -> failwith "err2"
            | [figId] ->
                // If one edge, check min and aax
                let figEdgeId = Map.find (prevFigId, figId) edgeFromVertexMap
                let min, max = edgeLengthRanges.[figEdgeId]
                min <= holeSegLen && holeSegLen <= max
            | _ ->
                // Check the max length of the path is sufficient
                let maxPathLen =
                    List.pairwise (prevFigId::path)
                    |> List.sumBy (fun (prev, next) ->
                        let figEdgeId = Map.find (prev, next) edgeFromVertexMap
                        let _, max = edgeLengthRanges.[figEdgeId]
                        max)
                holeSegLen <= maxPathLen)
        |> Seq.map List.last

    let rec placeFigVertex (partialMatch: int option array) (firstFigId: VertexId) (prevFigId: VertexId) (holeId: VertexId) =
        if holeId >= problem.Hole.Length then
            // Check a possible back edge
            match Map.tryFind (prevFigId, firstFigId) edgeFromVertexMap with
            | None -> Seq.singleton partialMatch
            | Some backEdge ->
                let holeSegLen = Geometry.segmentLength holeSegments.[holeId-1]
                let min, max = edgeLengthRanges.[backEdge]
                if min <= holeSegLen && holeSegLen <= max then
                    Seq.singleton partialMatch
                else
                    Seq.empty
        else
        let holeSegLen = Geometry.segmentLength holeSegments.[holeId-1]
        nextCandidates partialMatch prevFigId holeSegLen 
        |> Seq.collect (fun figId ->
            let adj = Map.find figId figAdjMap
            // For each adj, if it's already matched, the edge needs to conform
            let conforms =
                adj
                |> List.filter (fun adjId -> Option.isSome partialMatch.[adjId])
                |> List.forall (fun adjId ->
                    let adjEdgeId = Map.find (figId, adjId) edgeFromVertexMap
                    let min, max = edgeLengthRanges.[adjEdgeId]
                    let forcedSeg = (problem.Hole.[holeId],
                                     problem.Hole.[Option.get partialMatch.[adjId]])
                    let forcedLen = Geometry.segmentLength forcedSeg
                    min <= forcedLen && forcedLen <= max)
            if conforms then
                let partialMatch = Array.copy partialMatch
                partialMatch.[figId] <- Some holeId
                placeFigVertex partialMatch firstFigId figId (holeId + 1)
            else
                Seq.empty)
    // Place the first
    { 0..problem.Figure.Vertices.Length-1 }
    |> Seq.collect (fun firstFigId ->
        let partialMatch = Array.init problem.Figure.Vertices.Length (fun _ -> None)
        partialMatch.[firstFigId] <- Some 0
        placeFigVertex partialMatch firstFigId firstFigId 1)

let simulateOnFreePoints (problem: Problem) (figure: Figure) (freePoints: list<int>) =
    let freePoints = Array.ofList freePoints
    /// Take a random vertex and take a single move in a random direction
    let translateRandomFreeVertex (figure: Figure) =
        let rnd = Util.getRandom ()
        let vertexId = freePoints.[rnd.Next(freePoints.Length)]
        Neighbors.translateRandomCoordOfVertex figure vertexId
    let getNeighbor = (fun (_, fig) ->
        ("_translateRandomFreeVertex", translateRandomFreeVertex fig))
    let stepper = Solver.simulatedAnnealingStepper problem getNeighbor 10_000
    Some (Solver.runSolver stepper figure)

    

let vertexMatchSolve (problem: Problem) (bestDislikes: option<int>) (writeSolution: Model.Figure -> unit) =
    if problem.Hole.Length > problem.Figure.Vertices.Length  then
        // Perfect match not possible
        printfn $"Matcher can't solve this (#hole = {problem.Hole.Length}, #fig vertices = {problem.Figure.Vertices.Length})"
        ()
    else
        printfn $"Matcher tries free points {problem.Figure.Vertices.Length - problem.Hole.Length}"
        let isValid = Penalty.isValid problem
        let dislikes = Penalty.dislikes problem
        let solution =
            vertexMatchSequence problem
            |> Seq.map (fun partialMatch -> 
                let fig = 
                    { problem.Figure with
                        Vertices =
                            partialMatch 
                            |> Array.mapi (fun i isMatched ->
                                match isMatched with
                                | None -> problem.Figure.Vertices.[i]
                                | Some holeId -> problem.Hole.[holeId]) }
                let freePoints =
                    partialMatch
                    |> Array.toList
                    |> List.indexed
                    |> List.filter (fun (_, isMatched) -> Option.isNone isMatched)
                    |> List.map fst
                fig, freePoints
                )
            |> Seq.tryPick (fun (figure, freePoints) ->
                let figure = 
                    if List.isEmpty freePoints then
                        figure
                    else
                        match simulateOnFreePoints problem figure freePoints with
                        | None -> figure
                        | Some sol -> sol
                if isValid figure && dislikes figure = 0 then
                    Some figure
                else
                    None)
        match solution with
        | None ->
            printfn "VertexMatcher failed to find a match"
        | Some solution -> writeSolution solution
        











// Play with command line arguments
let problemPath = ref None
let problemNo = ref None
let gui = ref false
let writeToFile = ref false
let seed = ref (int DateTime.Now.Ticks)
let solution = ref None
let matcher = ref false

let argSpecs =
    [ "-pp", ArgType.String (fun p -> problemPath := Some p), "Path to problems"
    ; "-p", ArgType.Int (fun p -> problemNo := Some p), "Problem number"
    ; "-g", ArgType.Unit (fun () -> gui := true), "Show GUI"
    ; "-w", ArgType.Unit (fun () -> writeToFile := true), "Write solution to file"
    ; "-v", ArgType.Unit (fun () -> Util._verbose := true), "Verbose"
    ; "-seed", ArgType.Int (fun s -> seed := s), "Randomness seed to use"
    ; "-sol", ArgType.String (fun sol -> solution := Some sol), "Don't run, but score the input solution to the given problem"
    ; "-matcher", ArgType.Unit (fun () -> matcher := true), "Solve using Vertex Matcher"
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

let getBestCurrentSolution solutionDir = 
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    bestCurrentSolution dirinfo

let writeSolution solutionDir problem figure =
    let solutionText = Model.deparseSolution(Model.solutionOfFigure(figure))
    let dirinfo = IO.Directory.CreateDirectory solutionDir
    let dislikes = Penalty.dislikes problem figure
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
        let writeIfTold =
            if !writeToFile then
                writeSolution solutionDir problem
            else
                printSolution
        match !solution, !gui with
        | Some solFile, _ ->
            // Score an existing solution
            printfn $"Scoring solution {solFile}:"
            let figure = parseSolutionFile solFile
                        |> figureOfSolution problem
            printfn $"\t{Penalty.figurePenaltiesToString problem figure}"
            printfn $"Dislikes: {Penalty.dislikesPenalty problem figure}"
            0
        | _, true ->
            // Run GUI w normal solver
            GUI.showGui problem (writeSolution solutionDir problem)
        | None, false ->
        // We want to solve
        let bestSolution = getBestCurrentSolution solutionDir
        // match bestSolution with
        // | Some 0 ->
        //     printfn "Skipping problem %d, which has a 0-solution" problemNo
        //     0
        // | _ ->
        if !matcher then
            vertexMatchSolve problem bestSolution writeIfTold
        else
            FitInHole.solve problem bestSolution writeIfTold
        0
    | _ ->
        printfn "Must specify both problem path and problem"
        1
