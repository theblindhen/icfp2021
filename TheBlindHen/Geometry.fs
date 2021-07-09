module Geometry

open Model

exception Exception of string

/// Line segment
type Segment = Coord * Coord

/// Invariant that vertices V1-3 are sorted in increasing order of their Y-coordinate
/// constructor will sort input
// type Triangle = 
//     struct 
//         val V1: Coord
//         val V2: Coord
//         val V3: Coord
//         new(c1: Coord, c2: Coord, c3: Coord) = 
//             match [c1; c2; c2] |> List.sortBy (fun c -> c.Y) with
//             | [v1; v2; v3] -> { V1 = v1; V2 = v2; V3 = v3 }
//             | _ -> raise (Exception("Triangle.new: invalid input"))
//   //          | _ -> { V1= failwith "Triangle must be a triangle"; V2=failwith ""; V3=failwith ""} 

//         // member this.GetDistanceFrom(p: Coordinate) = 
//         //     let dX = pown (p.X - this.X) 2
//         //     let dY = pown (p.Y - this.Y) 2
            
//         //     dX + dY
//         //     |> sqrt
//     end

/// are sorted in increasing order of their Y-coordinate, then X-coordinate
// let sortSimplePolygon (vertices : Coord array) =
//     let unsortedEdges : (VertexId * VertexId) list = List.zip [0..(vertices.Length-1)] [1..(vertices.Length-1)] 
//     let edges = unsortedEdges
//                 |> List.sortBy (fun (vdx1, vdx2) -> min vertices.[vdx1].X vertices.[vdx2].X)
//                 |> List.sortBy (fun (vdx1, vdx2) -> min vertices.[vdx1].Y vertices.[vdx2].Y)
//     (vertices, edges)

// let innerCoordsOfSimplePolygon  vertices =
//     let edges



// let innerCoordsOfTriangle (triangle: Triangle) =

//     [triangle.Item1; triangle.Item2; triangle.Item3]



let segmentLengthSq ((p1, p2): Segment) =
    let dx, dy = p1.X - p2.X, p1.Y - p2.Y
    dx*dx + dy*dy
