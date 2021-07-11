module Neighbors

open Model

let directions = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]

let translateRandomCoord (problem: Problem) (figure: Figure) =
    let rnd = Util.getRandom ()
    let rndIndex = rnd.Next(figure.Vertices.Length)
    let (dx, dy) = directions.[rnd.Next(directions.Length)]
    let rndCord = Array.item rndIndex figure.Vertices
    let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
    let newFigure = copyFigureVerticies figure
    Array.set newFigure.Vertices rndIndex newCord
    Some (newFigure)

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
        |> Array.map (fun (i, _) -> (i, Graph.connectedComponentsWithoutVertex i adj))
    fun figure ->
        let noArticulationPoints = articulationPointsAndComponents.Length
        if noArticulationPoints > 0 then
            let rndArticulationPoint, components = articulationPointsAndComponents.[noArticulationPoints]
            let rndArticulationPointCoord = figure.Vertices.[rndArticulationPoint]
            // TODO: consider if it makes sense to pick the larger component in some cases
            let selection = List.minBy List.length components
            Some (Transformations.rotateSelectedVerticiesAround selection rndArticulationPointCoord figure)
        else None
 
let weightedChoice (choices: (float * (Problem -> 'a -> 'b option)) list) (problem: Problem) (param: 'a) : 'b option =
    let choices = List.map (fun (w, f) -> (w, f problem)) choices
    let totalWeight = List.sumBy fst choices
    let rnd = Util.getRandom ()
    let randomNumber = rnd.NextDouble() * totalWeight
    let rec iter acc =
        function
        // This can't happen if unless there are no choices given
        | [] -> failwith "Nowhere to go?!"
        // Make sure we always pick the last choice, even if there was
        // floating-point imprecision.
        | [ (_, f) ] -> f
        | (weight, f) :: tail ->
            let acc = acc + weight
            if randomNumber < acc then
                f
            else
                iter acc tail
    (iter 0.0 choices) param

/// This should be our main neighbors function that takes a balanced approach to
/// selecting a reasonable number of neighbors of each kind.
let balancedCollectionOfNeighbors (problem: Problem) =
    weightedChoice [
        4.0, translateRandomCoord;
        1.0, translateFullFigureRandomly;
        //0.1, rotateFullFigureAroundRandomPoint;
        //1.0, rotateRandomArticulationPoint
    ] problem
