module Neighbors

open Model

let directions = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]

let weightedChoice (choices: (float * 'a ) list) : 'a =
    let totalWeight = List.sumBy (fun (w,_) -> w) choices
    let rnd = Util.getRandom ()
    let randomNumber = rnd.NextDouble() * totalWeight
    let rec iter acc =
        function
        | [] -> failwith "Nowhere to go"
        // Make sure we always pick the last choice, even if there was
        // floating-point imprecision.
        | [ (_, e) ] -> e
        | (weight, e) :: tail ->
            let acc = acc + weight
            if randomNumber < acc then
                e
            else
                iter acc tail
    iter 0.0 choices

let getRandomBadVertexIdx (problem: Problem) =
    let badness = Penalty.vertexBadness problem
    fun fig ->
        let badness = badness fig
        let badChoices =
            badness
            |> Array.toList
            |> List.indexed
            |> List.filter (fun (_,pen) -> pen > 0.0)
            |> List.map (fun (idx, pen) -> (sqrt (pen), idx))
        match badChoices with
        | [] -> None
        | _  -> Some (weightedChoice badChoices)

let translateRandomCoordOfVertex (figure: Figure) (vertexIdx: int) =
    let rnd = Util.getRandom ()
    let (dx, dy) = directions.[rnd.Next(directions.Length)]
    let rndCord = Array.item vertexIdx figure.Vertices
    let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
    let newFigure = copyFigureVerticies figure
    Array.set newFigure.Vertices vertexIdx newCord
    Some (newFigure)

/// Take a random vertex and take a single move in a random direction
let translateRandomVertex (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let vertexIdx = rnd.Next(figure.Vertices.Length)
    translateRandomCoordOfVertex figure vertexIdx

/// Take a random vertex and take a single move in a random direction
let translateRandomBadVertex (problem: Problem) =
    let getRandomBadVertexIdx = getRandomBadVertexIdx problem
    fun fig ->
        match getRandomBadVertexIdx fig with
        | None -> None
        | Some idx -> translateRandomCoordOfVertex fig idx

let translateRandomCoordOfVertexMult (problem: Problem) (moves: int) (figure: Figure) (vertexIdx: int) =
    let getNeighbor = (fun (_, fig) ->
        ("_translateRandomCoordMultiple", translateRandomCoordOfVertex fig vertexIdx))
    // TODO: Temperature control of the local simulated annealing?
    let stepper = Solver.simulatedAnnealingStepper problem getNeighbor moves
    Some (Solver.runSolver stepper figure)

/// Take a random vertex and take a multiple moves according to a meta-heuritic
let translateRandomVertexMultiple (problem: Problem) (moves: int) (figure: Figure) =
    let rnd = Util.getRandom ()
    rnd.Next(figure.Vertices.Length)
    |> translateRandomCoordOfVertexMult problem moves figure

/// Take a random vertex and take a multiple moves according to a meta-heuritic
let translateRandomBadVertexMultiple (problem: Problem) (moves: int) =
    let getRandomBadVertexIdx = getRandomBadVertexIdx problem
    fun fig ->
        match getRandomBadVertexIdx fig with
        | None -> None
        | Some idx -> translateRandomCoordOfVertexMult problem moves fig idx

let translateFullFigureRandomly (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let delta = directions.[rnd.Next(directions.Length)]
    Some (Transformations.translateVerticies delta figure)

let translateFullFigureRandomlyLong (l: int) (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let (x, y) = directions.[rnd.Next(directions.Length)]
    Some (Transformations.translateVerticies (x * l, y * l) figure)

let translateFullFigureBest (problem: Problem) = 
    let penalty = Penalty.figurePenalty problem
    fun fig ->
        Some (
            directions 
            |> Array.map (fun dir -> Transformations.translateVerticies dir fig)
            |> Array.map (fun fig -> (fig, penalty fig))
            |> Array.minBy snd
            |> fst
        )

let rotateFullFigureAroundRandomPoint (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let rndIndex = rnd.Next(figure.Vertices.Length)
    let rndCord = Array.item rndIndex figure.Vertices
    Some (Transformations.rotateVerticiesAround rndCord figure)

let rotateFullFigureAroundBestPoint (problem: Problem) =
    let penalty = Penalty.figurePenalty problem
    fun figure ->
        let bestRot =
            figure.Vertices
            |> Array.toList
            |> List.collect (fun c ->
                Transformations.rotateVerticiesAround3 c figure)
            |> List.minBy penalty
        if penalty bestRot < penalty figure then Some bestRot else None

let rotateRandomArticulationPoint (problem: Problem) =
    let rnd = Util.getRandom ()
    let adj, isArticulationPoint = Graph.getArticulationPoints problem.Figure
    let articulationPointsAndComponents =
        isArticulationPoint
        |> Array.mapi (fun i v -> (i, v))
        |> Array.filter snd
        |> Array.map (fun (i, _) -> (i, Graph.connectedComponentsWithoutVertices adj [i]))
    fun figure ->
        let noArticulationPoints = articulationPointsAndComponents.Length
        if noArticulationPoints > 0 then
            let rndArticulationPoint, components = articulationPointsAndComponents.[rnd.Next(noArticulationPoints)]
            let rndArticulationPointCoord = figure.Vertices.[rndArticulationPoint]
            // TODO: consider if it makes sense to pick the larger component in some cases
            let selection = List.minBy List.length components
            Some (Transformations.rotateSelectedVerticiesAround selection rndArticulationPointCoord figure)
        else None

let rotateRandomArticulationPointSet (problem: Problem) =
    let rnd = Util.getRandom ()
    let adj = Graph.adjacencyMatrix problem.Figure
    fun figure ->
        let articulationPointSets = Graph.findArticulationPointSets adj figure
        if not(List.isEmpty articulationPointSets) then
            let (c, components) = articulationPointSets.[rnd.Next(articulationPointSets.Length)]
            let rndComponent = components.[rnd.Next(components.Length)]
            Some (Transformations.rotateSelectedVerticiesAround rndComponent c figure) 
        else None

let mirrorAcrossRandomVerticalCutLine (problem: Problem) =
    let rnd = Util.getRandom ()
    let adj = Graph.adjacencyMatrix problem.Figure
    fun figure ->
        let verticalCutComponents = Graph.findVerticalCutComponents adj figure
        if not(List.isEmpty verticalCutComponents) then
            let (x, components) = verticalCutComponents.[rnd.Next(verticalCutComponents.Length)]
            let rndComponent = components.[rnd.Next(components.Length)]
            Some (Transformations.mirrorSelectedVerticiesVertically rndComponent x figure)
        else None

let mirrorAcrossBestVerticalCutLine (problem: Problem) =
    let adj = Graph.adjacencyMatrix problem.Figure
    let penalty = Penalty.figurePenalty problem
    fun figure ->
        let bestMirror =
            Graph.findVerticalCutComponents adj figure
            |> List.collect (fun (x, components) ->
                List.map (fun selection ->
                    Transformations.mirrorSelectedVerticiesVertically selection x figure) components)
            |> List.sortBy penalty
            |> List.tryHead
        match bestMirror with
        | Some bestMirror when penalty bestMirror < penalty figure -> Some bestMirror 
        | _ -> None

let mirrorAcrossRandomHorizontalCutLine (problem: Problem) =
    let rnd = Util.getRandom ()
    let adj = Graph.adjacencyMatrix problem.Figure  
    fun figure ->
        let horizontalCutComponents = Graph.findHorizontalCutComponents adj figure
        if not(List.isEmpty horizontalCutComponents) then
            let (y, components) = horizontalCutComponents.[rnd.Next(horizontalCutComponents.Length)]
            let rndComponent = components.[rnd.Next(components.Length)]
            Some (Transformations.mirrorSelectedVerticiesHorizontally rndComponent y figure)
        else None

let mirrorAcrossRandomLegalCutLine (problem: Problem) =
    let rnd = Util.getRandom ()
    let articulationPairs = Graph.findArticulationPairs problem.Figure
    let penaltyEdgeLength = Penalty.penaltyEdgeLengthSqSum problem

    fun figure ->
        if not(List.isEmpty articulationPairs) then
            let ((cid1,cid2), cutComponents) = articulationPairs.[rnd.Next(articulationPairs.Length)]
            let (c1,c2) = (problem.Figure.Vertices.[cid1], problem.Figure.Vertices.[cid2])
            let selection = List.minBy List.length cutComponents
            let figure = Transformations.mirrorSelectedVerticiesAcrossSegment selection (c1,c2) problem.Figure
            if penaltyEdgeLength figure = 0.0 then
                Some (figure)
            else 
                None
        else None

let mirrorAcrossBestHorizontalCutLine (problem: Problem) =
    let adj = Graph.adjacencyMatrix problem.Figure
    let penalty = Penalty.figurePenalty problem
    fun figure ->
        let bestMirror =
            Graph.findHorizontalCutComponents adj figure
            |> List.collect (fun (y, components) ->
                List.map (fun selection ->
                    Transformations.mirrorSelectedVerticiesHorizontally selection y figure) components)
            |> List.sortBy penalty
            |> List.tryHead
        match bestMirror with
        | Some bestMirror when penalty bestMirror < penalty figure -> Some bestMirror 
        | _ -> None

let mustImprovePenalty (f: Problem -> (Figure -> option<Figure>)) (problem: Problem) =
    let f = f problem
    let penalties = Penalty.figurePenalty problem
    fun figure ->
        match f figure with
        | None -> None
        | Some figure' ->
            if penalties figure' < penalties figure then Some figure' else None

let repeatWhileImproving (f: Problem -> (Figure -> option<Figure>)) (problem: Problem) =
    let penalty = Penalty.figurePenalty problem
    let f = f problem
    fun fig ->
        let rec inner fig =
            match f fig with
            | None -> Some fig
            | Some fig' ->
                printfn "Penalty: %f" (penalty fig')
                if penalty fig' < penalty fig then inner fig' else Some (fig)
        inner fig

let weightedFunChoice (choices: ((float -> float) * string * ('a -> 'b option)) list) ((r, a): float * 'a) : string * 'b option =
    let totalWeight = List.sumBy (fun (w,_,_) -> w r) choices
    let rnd = Util.getRandom ()
    let randomNumber = rnd.NextDouble() * totalWeight
    let rec iter acc =
        function
        // This can't happen if unless there are no choices given
        | [] -> failwith "Nowhere to go?!"
        // Make sure we always pick the last choice, even if there was
        // floating-point imprecision.
        | [ (_, desc, f) ] -> (desc, f a)
        | (w, desc, f) :: tail ->
            let acc = acc + w r
            if randomNumber < acc then
                (desc, f a)
            else
                iter acc tail
    iter 0.0 choices

let constWeight (w: float) = fun _ -> w
let stopAfter (w: float) f = fun r -> if r > w then 0.0 else f r
let expDampning (wStart: float) (wEnd: float) = fun r -> (wStart - wEnd) * exp (-r * 100.0) + wEnd

let mayIncreasePenaltyByFactor (factor: float) (f: Problem -> (Figure -> Figure option)) (problem: Problem) =
    let penalty = Penalty.figurePenalty problem
    let f = f problem
    fun fig ->
        match f fig with
        | None -> None
        | Some fig' -> if penalty fig' < factor * penalty fig then Some fig' else None

/// This should be our main neighbors function that takes a balanced approach to
/// selecting a reasonable number of neighbors of each kind.
let balancedCollectionOfNeighbors (problem: Problem) =
    weightedFunChoice [
        // NOTE: partial neighbor functions should preceed total neighbor functions
 
        // Partial neighbor functions
        constWeight 0.001, "rot articulation point", mustImprovePenalty rotateRandomArticulationPoint problem;
        constWeight 0.001, "mirror vertical cut (try all)", mirrorAcrossBestVerticalCutLine problem;
        constWeight 0.001, "mirror horizontal cut (try all)", mirrorAcrossBestHorizontalCutLine problem;
        constWeight 0.001, "rot full fig (try all)", rotateFullFigureAroundBestPoint problem;
        // constWeight 0.001, "mirror across random cut segment", mirrorAcrossRandomLegalCutLine problem;
        // constWeight 0.001, "rot articulation pntset", rotateRandomArticulationPointSet problem;

        // Total neighbor functions
        constWeight 4.0, "single step", translateRandomBadVertex problem;
        // 1.5, "10 steps", translateRandomBadVertexMultiple problem 10;
        // 0.5, "25 steps", translateRandomBadVertexMultiple problem 25;
        // 0.2, "50 steps", translateRandomBadVertexMultiple problem 50;

        constWeight 0.1, "trans full fig", translateFullFigureRandomly problem;
        // 1.0, "trans full fig long", repeatWhileImproving translateFullFigureBest problem;
        // constWeight 0.001, "rot full fig", rotateFullFigureAroundRandomPoint problem;

        // large transformations that are only active for the first 10 iterations
        stopAfter 0.001 (constWeight 100.0), "trans full fig (long)", mayIncreasePenaltyByFactor 10.0 (translateFullFigureRandomlyLong 10) problem;
        stopAfter 0.001 (constWeight 100.0), "rot full fig", mayIncreasePenaltyByFactor 10.0 rotateFullFigureAroundRandomPoint problem;
    ]
