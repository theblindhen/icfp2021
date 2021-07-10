module Neighbors

open Model

let translateRandomCoord (rnd: System.Random) (figure: Figure) =
    let rndIndex = rnd.Next(figure.Vertices.Length)
    List.map (fun (dx, dy) -> 
        let rndCord = Array.item rndIndex figure.Vertices
        let newCord = Coord(rndCord.X + dx, rndCord.Y + dy)
        // TODO: it's wasteful to copy the edges and not modify them
        let newFigure = copyFigure figure
        Array.set newFigure.Vertices rndIndex newCord
        newFigure) [(1, 0); (0, 1); (-1, 0); (0, -1)]