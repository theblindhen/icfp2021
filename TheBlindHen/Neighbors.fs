module Neighbors

open Model

let directions = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]

let translateRandomCoord (rnd: System.Random) (figure: Figure) =
    let rndIndex = rnd.Next(figure.Vertices.Length)
    let (dx, dy) = directions.[rnd.Next(directions.Length)]
    let rndCord = Array.item rndIndex figure.Vertices
    let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
    let newFigure = copyFigureVerticies figure
    Array.set newFigure.Vertices rndIndex newCord
    newFigure

let translateFullFigureRandomly (rnd: System.Random) (figure: Figure) =
    let delta = directions.[rnd.Next(directions.Length)]
    Transformations.translateVerticies delta figure

let rotateFullFigureAroundRandomPoint (rnd: System.Random) (figure: Figure) =
    let rndIndex = rnd.Next(figure.Vertices.Length)
    let rndCord = Array.item rndIndex figure.Vertices
    Transformations.rotateVerticiesAround rndCord figure
 
let weightedChoice (choices: (float * (System.Random -> 'a -> 'b)) list) (rnd: System.Random) (param: 'a) : 'b =
    let totalWeight = List.sumBy fst choices
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
    (iter 0.0 choices) rnd param

/// This should be our main neighbors function that takes a balanced approach to
/// selecting a reasonable number of neighbors of each kind.
let balancedCollectionOfNeighbors (rnd: System.Random) (figure: Figure) =
    weightedChoice [
        4.0, translateRandomCoord;
        1.0, translateFullFigureRandomly;
//      0.1, rotateFullFigureAroundRandomPoint;
    ] rnd figure
