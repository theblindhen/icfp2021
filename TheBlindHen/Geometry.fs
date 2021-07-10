module Geometry

open Model

exception Exception of string

/// Line segment
type Segment = Coord * Coord

type Vector = 
    struct 
        val X: float
        val Y: float
        new(x: float, y: float) = { X = x; Y = y }

        member this.Length = 
            sqrt(this.X*this.X + this.Y*this.Y)
    end

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

let EPSILON = 0.001

let vectorOfSegment ((s1, s2): Segment) =
    Vector (float (s1.X - s2.X), float (s1.Y - s2.Y)) 

let vectorDotProduct (v1: Vector) (v2: Vector) =
    v1.X * v2.X + v1.Y * v2.Y

let vectorsAreParallel (v1: Vector) (v2: Vector) =
    let v2hat = Vector(-v2.Y, v2.X)
    abs (vectorDotProduct v1 v2hat) < EPSILON

/// Solve a 2x2 matrix problem by inversion.
/// Returns None if the matrix is singular.
/// Note that there may still be a solution, if the column space is
/// 1-dimensional and the target vector happens to fall within that.
let solveByInversion(row1: Vector, row2: Vector) (target: Vector) =
    let d = row1.X * row2.Y - row1.Y * row2.X
    if abs(d) < EPSILON then
        None
    else
        let sX = target.X * row2.Y - target.Y * row2.X
        let sY = target.X * row1.Y - target.Y * row1.X
        Some (Vector (sX/d, sY/d))
        
type SegmentIntersect = 
      /// Multiplier on base segment to get the intersection point
    | Point of float
      /// Multipliers on base segment for start/end of the overlap
    | Overlap of float * float

let segmentsIntersect (seg1 : Segment) (seg2: Segment) : SegmentIntersect option =
    let v1 = vectorOfSegment seg1
    let v2 = vectorOfSegment seg2
    let p1,_ = seg1
    let p2,q2 = seg2
    let startDiff = Vector(float (p2.X - p1.X), float (p2.Y - p1.Y))
    match solveByInversion (v1, v2) startDiff with
    | Some sol -> Some (Point (-sol.X))
    | None ->
        // The segments are parallel.
        // They are then on the same line exactly when startDiff is parallel to v1
        // TODO: Refactor to save a sqrt
        if vectorsAreParallel startDiff v1 then
            Some (Overlap (float(p1.X - p2.X)/v1.X, float(p1.X - q2.X)/v1.X))
        else
            None

let segmentIntersectionList (seg: Segment) (intersectors: Segment list) : SegmentIntersect list =
    intersectors 
    |> List.choose (segmentsIntersect seg)
    |> List.map (function
        | Overlap (a,b) ->
            let a, b = 
                if a < b then
                    max a 0., min b 1.
                else
                    max b 0., min a 1.
            if abs(a - b) < EPSILON then
                Point a // TODO: Test for this case
            else
                Overlap (a, b)
        | ints -> ints)
    |> List.filter (function
        | Point a -> a >= 0. && a <= 1.
        | Overlap (a,b) -> a <= 1. && b >= 0. ) 

let segmentDecomposition (seg: Segment) (simplePolygon: Segment list) : SegmentIntersect list =
    let intersections = segmentIntersectionList seg simplePolygon
    let overlapEndpoints : HashSet<int> =
        intersections
        |> List.collect (function
                         | Overlap (a,b) -> [ int (round a); int (round b) ]
                         | _ -> [])
        |> HashSet.ofList
    intersections
    |> List.filter (function
        | Overlap _ -> true
        | Point a ->
            not (HashSet.contains (int (round a)) overlapEndpoints))
    |> List.sortBy (fun ints ->
        match ints with
        | Point a -> a
        | Overlap (a,_) -> a)


// /// A structure for precomputed set of points in the hole
// type HolePoints = {
//     Arr: bool[,]
//     Dx: int
//     Dy: int
// }

// /// Return the set of points of the hole of the problem
// let getHolePoints (problem: Problem) =
//     let holeSegs = holeSegments problem
//     let cmin, cmax = holeBoundingBox problem
//     let arr = Array2D.init (cmax.X - cmin.X + 1) (cmax.Y - cmin.Y + 1) (fun _ _ -> false)
//     for y in cmin.Y .. cmax.Y do
//         let seg = (Coord (cmin.X, y), Coord (cmax.X, y))
//         let lastCross = ref -1.
//         let inHole = ref false
//         segmentDecomposition seg holeSegs
//         |> List.iter (fun ints ->
//             match ints with
//             | Point a ->
//                 if !inHole then
//                     let startX = ceil (!lastCross-EPSILON)
//                     let endX = floor (a + EPSILON)
//                     for x in  startX .. endX  do
//                         if float x >= cmin.X && float x <= cmax.X then
//                             arr[x - cmin.X, y - cmin.Y] = true
//                 )

//     { Arr = arr
//       Dx = cmin.X
//       Dy = cmin.Y }

// /// Is the given coord in the hole
// let inHole (c: Coord) (hole: HolePoints) =
//     let lX = c.X - hole.Dx
//     let lY = c.Y - hole.Dy
//     if lX < 0 || lX >= Array2D.length1 hole.Arr || lY < 0 || lY >= Array2D.length2 hole.Arr then
//         false
//     else 
//         hole.Arr.[lX, lY]
