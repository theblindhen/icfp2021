module Neighbors

open Model

let translateRandomCoord (rnd: System.Random) (figure: Figure) =
    let rndIndex = rnd.Next(figure.Vertices.Length)
    List.map (fun (dx, dy) -> 
        let rndCord = Array.item rndIndex figure.Vertices
        let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
        let newFigure = copyFigureVerticies figure
        Array.set newFigure.Vertices rndIndex newCord
        newFigure) [(1, 0); (0, 1); (-1, 0); (0, -1)]

let translateFullFigureRandomly (rnd: System.Random) (figure: Figure) =
    let choices = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]
    let delta = choices.[rnd.Next(choices.Length)]
    Transformations.translateVerticies delta figure

/// This should be our main neighbors function that takes a balanced approach to
/// selecting a reasonable number of neighbors of each kind.
let balancedCollectionOfNeighbors (rnd: System.Random) (figure: Figure) =
    seq {
        yield! translateRandomCoord rnd figure
        yield translateFullFigureRandomly rnd figure
    }
    |> List.ofSeq
