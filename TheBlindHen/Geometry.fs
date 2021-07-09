module Geometry

open Model

type Edge = Coord * Coord

type Triangle = Coord * Coord * Coord

// let innerCoordsOfTriangle (triangle: Triangle) =

//     [triangle.Item1; triangle.Item2; triangle.Item3]



let edgeLengthSq ((p1, p2): Edge) =
    let dx, dy = p1.X - p2.X, p1.Y - p2.Y
    dx*dx + dy*dy
