module Transformations

let mapSelectedVerticies (selection: int list) (f: Model.Coord -> Model.Coord) =
    Model.mapiFigureVerticies (fun i c -> if List.contains i selection then f c else c)

// translate all verticies
let translateVerticies (dx, dy : int) =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.X + dx, c.Y + dy))

let translateSelectedVerticies (selection: int list) (dx, dy : int) =
    mapSelectedVerticies selection (fun c -> Model.Coord(c.X + dx, c.Y + dy))

// rotate all verticies 90 degrees clockwise around (0, 0)
let rotateVerticies =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.Y, -c.X))

let rotateSelectedVerticies (selection: int list) =
    mapSelectedVerticies selection (fun c -> Model.Coord(c.Y, -c.X))

let rotateVerticiesAround (ox, oy : int) =
    translateVerticies (-ox, -oy) >> rotateVerticies >> translateVerticies (ox, oy)

let rotateSelectedVerticiesAround (selection: int list) (ox, oy : int) =
    translateSelectedVerticies selection (-ox, -oy)
    >> rotateSelectedVerticies selection 
    >> translateSelectedVerticies selection (ox, oy)