module Transformations

open System

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

/// Angle is expressed in degrees
let rotateSelectedVerticiesByAngle (selection: int list) (angle: float)  =
    let rad = angle * (Math.PI / 180.0)
    mapSelectedVerticies selection (fun c ->
        let x' = int(float(c.X) * Math.Cos(rad) - float(c.Y) * Math.Sin(rad))
        let y' = int(float(c.Y) * Math.Cos(rad) + float(c.X) * Math.Sin(rad))
        Model.Coord(x', y'))

let rotateVerticiesAround (ox, oy : int) =
    translateVerticies (-ox, -oy) >> rotateVerticies >> translateVerticies (ox, oy)

let rotateSelectedVerticiesAround (selection: int list) (origo: Model.Coord) =
    translateSelectedVerticies selection (-origo.X, -origo.Y)
    >> rotateSelectedVerticies selection
    >> translateSelectedVerticies selection (origo.X, origo.Y)

/// Angle is expressed in degrees
let rotateSelectedVerticiesAroundByAngle (selection: int list) (origo: Model.Coord) (angle: float) =
    translateSelectedVerticies selection (-origo.X, -origo.Y)
    >> rotateSelectedVerticiesByAngle selection angle
    >> translateSelectedVerticies selection (origo.X, origo.Y)