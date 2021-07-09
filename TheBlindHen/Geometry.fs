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

let segmentLengthSq ((p1, p2): Segment) =
    let dx, dy = p1.X - p2.X, p1.Y - p2.Y
    dx*dx + dy*dy

/// sort segment coordinates in increasing order of their Y-coordinate, then X-coordinate
let sortSegment ((c1,c2): Segment) : Segment =
    if c2.Y < c1.Y || (c2.Y = c1.Y && c2.X < c1.X) then
        (c2,c1)
    else
        (c1,c2) 

/// sort each segment coordinates in increasing order of their Y-coordinate, then X-coordinate
/// and sort segments in increasing order of their Y-coordinate, then X-coordinate
let sortSegments (segments : Segment list) =
    segments
    |> List.map sortSegment
    |> List.sortBy (fun (c1, c2) -> min c1.X c2.X)
    |> List.sortBy (fun (c1, c2) -> min c1.Y c2.Y)

/// assumes segments are sorted
// let innerCoordsOfSimplePolygon  segments =
//     let edges

// let solveTwoByTwo (row1: Coord, row2: Coord) (target: Coord) =
//     let D = row1.X * row2.Y - row1.Y * row2.X
//     if D === 0 then
//         None
//     else
//         let inv1 = 

type SegmentOverlap =
    | StartFirst of float
    | StartSecond of float
type SegmentIntersection =
    | Parallel
    | Overlap of SegmentOverlap
    | Point of float * float    // Multiplier of first and second segment vector for point

let segmentsIntersect (seg1 : Segment) (seg2: Segment) : SegmentIntersection =
    Parallel




