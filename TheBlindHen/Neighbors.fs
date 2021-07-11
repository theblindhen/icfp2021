module Neighbors

open Model

let directions = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]

let translateRandomCoordOfVertex (problem: Problem) (figure: Figure) (vertexIdx: int) =
    let rnd = Util.getRandom ()
    let (dx, dy) = directions.[rnd.Next(directions.Length)]
    let rndCord = Array.item vertexIdx figure.Vertices
    let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
    let newFigure = copyFigureVerticies figure
    Array.set newFigure.Vertices vertexIdx newCord
    Some (newFigure)

/// Take a random vertex and take a single move in a random direction
let translateRandomCoord (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let vertexIdx = rnd.Next(figure.Vertices.Length)
    translateRandomCoordOfVertex problem figure vertexIdx

/// Take a random vertex and take a multiple moves according to a meta-heuritic
let translateCoordMultiple (problem: Problem) (moves: int) (figure: Figure) =
    let rnd = Util.getRandom ()
    let vertexIdx = rnd.Next(figure.Vertices.Length)
    let getNeighbor = (fun fig -> translateRandomCoordOfVertex problem fig vertexIdx)
    // TODO: Temperature control of the local simulated annealing?
    let stepper = Solver.simulatedAnnealingStepper problem getNeighbor moves
    Solver.runSolver stepper figure


let translateFullFigureRandomly (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let delta = directions.[rnd.Next(directions.Length)]
    Some (Transformations.translateVerticies delta figure)

let rotateFullFigureAroundRandomPoint (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let rndIndex = rnd.Next(figure.Vertices.Length)
    let rndCord = Array.item rndIndex figure.Vertices
    Some (Transformations.rotateVerticiesAround rndCord figure)

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

let weightedChoice (choices: (float * ('a -> 'b option)) list) (param: 'a) : 'b option =
    let totalWeight = List.sumBy fst choices
    let rnd = Util.getRandom ()
    let randomNumber = rnd.NextDouble() * totalWeight
    let rec iter acc =
        function
        // This can't happen if unless there are no choices given
        | [] -> failwith "Nowhere to go?!"
        // Make sure we always pick the last choice, even if there was
        // floating-point imprecision.
        | [ (_, f) ] -> f param
        | (weight, f) :: tail ->
            let acc = acc + weight
            if randomNumber < acc then
                match f param with
                | None -> iter acc tail
                | x -> x
            else
                iter acc tail
    iter 0.0 choices

/// This should be our main neighbors function that takes a balanced approach to
/// selecting a reasonable number of neighbors of each kind.
let balancedCollectionOfNeighbors (problem: Problem) =
    weightedChoice [
        // NOTE: partial neighbor functions should preceed total neighbor functions
 
        // Partial neighbor functions
        //1.0, (rotateRandomArticulationPoint problem);
        //1.0, (mirrorAcrossRandomVerticalCutLine problem);
        //1.0, (mirrorAcrossRandomHorizontalCutLine problem);

        // Total neighbor functions
        4.0, (translateRandomCoord problem);
        1.0, (translateFullFigureRandomly problem);
        1.0, (rotateFullFigureAroundRandomPoint problem);
    ]
