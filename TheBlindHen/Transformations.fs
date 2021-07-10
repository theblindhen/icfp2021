module Transformations

// translate all verticies
let translateVerticies (dx, dy : int) =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.X + dx, c.Y + dy))

// rotate all verticies 90 degrees clockwise around (0, 0)
let rotateVerticies (figure: Model.Figure) =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.Y, -c.X))