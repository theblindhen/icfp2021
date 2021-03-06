module Transformations

open System

let mapSelectedVertices (selection: int list) (f: Model.Coord -> Model.Coord) =
    Model.mapiFigureVerticies (fun i c -> if List.contains i selection then f c else c)

// translate all verticies
let translateVerticies (dx, dy : int) =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.X + dx, c.Y + dy))

let translateSelectedVerticies (selection: int list) (dx, dy : int) =
    mapSelectedVertices selection (fun c -> Model.Coord(c.X + dx, c.Y + dy))

// rotate all verticies 90 degrees clockwise around (0, 0)
let rotateVerticies =
    Model.mapFigureVerticies (fun c -> Model.Coord(c.Y, -c.X))

let rotateSelectedVerticies (selection: int list) =
    mapSelectedVertices selection (fun c -> Model.Coord(c.Y, -c.X))

/// Angle is expressed in degrees
let rotateSelectedVerticiesByAngle (selection: int list) (angle: float)  =
    let rad = angle * (Math.PI / 180.0)
    mapSelectedVertices selection (fun c ->
        let x' = int(round(float(c.X) * Math.Cos(rad) - float(c.Y) * Math.Sin(rad)))
        let y' = int(round(float(c.Y) * Math.Cos(rad) + float(c.X) * Math.Sin(rad)))
        Model.Coord(x', y'))

let rotateVerticiesAround (origo: Model.Coord) =
    translateVerticies (-origo.X, -origo.Y) >> rotateVerticies >> translateVerticies (origo.X, origo.Y)

let rotateVerticiesAround3 (origo: Model.Coord) figure =
    let rot fig = rotateVerticiesAround origo fig
    let rot1 = rot figure
    let rot2 = rot rot1
    let rot3 = rot rot2
    [rot1; rot2; rot3]

let rotateSelectedVerticiesAround (selection: int list) (origo: Model.Coord) =
    translateSelectedVerticies selection (-origo.X, -origo.Y)
    >> rotateSelectedVerticies selection
    >> translateSelectedVerticies selection (origo.X, origo.Y)

/// Angle is expressed in degrees
let rotateSelectedVerticiesAroundByAngle (selection: int list) (origo: Model.Coord) (angle: float) =
    translateSelectedVerticies selection (-origo.X, -origo.Y)
    >> rotateSelectedVerticiesByAngle selection angle
    >> translateSelectedVerticies selection (origo.X, origo.Y)

let mirrorSelectedVerticiesVertically (selection: int list) (x: int) =
    mapSelectedVertices selection (fun c -> Model.Coord(-c.X + 2 * x, c.Y))

let mirrorSelectedVerticiesHorizontally (selection: int list) (y: int) =
    mapSelectedVertices selection (fun c -> Model.Coord(c.X, -c.Y + 2 * y))

/// Reflect selected vertices across the line through c1 and c2
/// using the answer from https://stackoverflow.com/questions/8954326/how-to-calculate-the-mirror-point-along-a-line
let mirrorSelectedVerticiesAcrossSegment (selection: int list) (c1 : Model.Coord, c2 : Model.Coord) =
    // represent the line by Ax + By + C = 0
    let (a,b) = (c2.Y - c1.Y, -(c2.X - c1.X))
    let c = -a * c1.X - b * c1.Y
    let m = sqrt (float (a * a + b * b))
    let (a',b',c') = (float a / m, float b / m, float c / m)

    let mirror (p : Model.Coord) =
        let d = a' * float p.X + b' * float p.Y + c'
        let px' = float p.X - 2.0 * a' * d
        let py' = float p.Y - 2.0 * b' * d
        Model.Coord (int (round px'), int (round py')) 

    mapSelectedVertices selection mirror