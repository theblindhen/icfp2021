module Geometry

open Model

exception Exception of string
        
type Direction = CW | CCW

/// Line segment
type Segment = Coord * Coord

let stringOfSegment (c1: Coord, c2: Coord) =
    $"({c1})--({c2})"

type FloatVector = 
    struct 
        val X: float
        val Y: float
        new(x: float, y: float) = { X = x; Y = y }

        member this.Length = 
            sqrt(this.X*this.X + this.Y*this.Y)

        member this.LengthSq = 
            this.X*this.X + this.Y*this.Y

        member this.Hat =
            FloatVector(-this.Y, this.X)
    end

type IntVector = 
    struct 
        val X: int
        val Y: int
        new(x: int, y: int) = { X = x; Y = y }

        member this.LengthSq =
            this.X*this.X + this.Y*this.Y

        member this.Hat =
            IntVector(-this.Y, this.X)

        member this.ToFloat =
            FloatVector(float this.X, float this.Y)
    end

let EPSILON = 0.00001
let isZero (x: float) = abs (x) < EPSILON
let isOne (x: float) = isZero (x - 1.0)

let intVectorOfSegment ((s1, s2): Segment) =
    IntVector (s2.X - s1.X, s2.Y - s1.Y) 

let floatVectorOfSegment ((s1, s2): Segment) =
    FloatVector (float (s2.X - s1.X), float (s2.Y - s1.Y)) 

let intVectorDotProduct (v1: IntVector) (v2: IntVector) =
    v1.X * v2.X + v1.Y * v2.Y

let floatVectorDotProduct (v1: FloatVector) (v2: FloatVector) =
    v1.X * v2.X + v1.Y * v2.Y

// TODO: These should all operate on Segments to be ints

let intVectorsParallel (v1: IntVector) (v2: IntVector) =
    // Morally intDotProduct v1 v2.Hat
    -v1.X * v2.Y + v1.Y * v2.X = 0

let floatVectorsParallel (v1: FloatVector) (v2: FloatVector) =
    isZero (-v1.X * v2.Y + v1.Y * v2.X )

let intVectorDeterminant (v1: IntVector) (v2: IntVector) =
    v1.X * v2.Y - v1.Y * v2.X

let floatVectorDeterminant (v1: FloatVector) (v2: FloatVector) =
    v1.X * v2.Y - v1.Y * v2.X

let _intDeterminantToDirection (d: int) : Direction =
    if d > 0 then CCW else CW

let _floatDeterminantToDirection (d: float) : Direction =
    if d > 0.0 then CCW else CW

let intVectorsDirection (v1: IntVector) (v2: IntVector) : Direction =
    _intDeterminantToDirection (intVectorDeterminant v1 v2)

let floatVectorsDirection (v1: FloatVector) (v2: FloatVector) : Direction =
    _floatDeterminantToDirection (floatVectorDeterminant v1 v2)

let intAddVectors (v1: IntVector) (v2: IntVector): IntVector =
    IntVector (v1.X + v2.X, v1.Y + v2.Y)

let floatAddVectors (v1: FloatVector) (v2: FloatVector): FloatVector =
    FloatVector (v1.X + v2.X, v1.Y + v2.Y)

let intScaleVector (v: IntVector) (s: int) =
    IntVector (v.X * s, v.Y * s)

let floatScaleVector (v: FloatVector) (s: float) =
    FloatVector (v.X * s, v.Y * s)

let floatReflectVector (v: FloatVector) (l: FloatVector): FloatVector =
    floatAddVectors (floatScaleVector l (2.0 * (floatVectorDotProduct v l) / (floatVectorDotProduct l l)))
                    (floatScaleVector v (-1.0))

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

/// Solve a 2x2 matrix problem by inversion.
/// Returns Some (intSol, d) if the matrix is regular, where intSol/d is the
/// solution, and d is the (non-zero) determinant of the matrix.
/// Returns None if the matrix is singular.
/// Note that there may still be a solution, if the column space is
/// 1-dimensional and the target vector happens to fall within that.
let solveByInversion(row1: IntVector, row2: IntVector) (target: IntVector) =
    let d = intVectorDeterminant row1 row2
    if d = 0 then
        None
    else
        let sX = target.X * row2.Y - target.Y * row2.X
        let sY = target.X * row1.Y - target.Y * row1.X
        Some (IntVector (sX, sY), d)

type SegmentIntersect = 
      /// Multiplier on seg1 to get the intersection point,
      /// Dir from seg1 to seg2
      /// Multiplier on seg2 to get the intersection point
    | Point of float * Direction * float
      /// Multipliers on base segment for start/end of the overlap
    | Overlap of float * float

let segmentsIntersect (seg1 : Segment) (seg2: Segment) : SegmentIntersect option =
    let v1 = intVectorOfSegment seg1
    let v2 = intVectorOfSegment seg2
    let p1,_ = seg1
    let p2,q2 = seg2
    let startDiff = IntVector(p2.X - p1.X, p2.Y - p1.Y)
    match solveByInversion (v1, v2) startDiff with
    | Some (sol, det) -> Some (Point ((float sol.X)/(float det),
                                      _intDeterminantToDirection det, 
                                      (float sol.Y)/(float det)))
    | None ->
        // The segments are parallel.
        // They are then on the same line exactly when startDiff is parallel to v1
        if intVectorsParallel startDiff v1 then
            if v1.X <> 0 then
                Some (Overlap (-float(p1.X - p2.X)/(float v1.X), -float(p1.X - q2.X)/(float v1.X)))
            else
                Some (Overlap (-float(p1.Y - p2.Y)/(float v1.Y), -float(p1.Y - q2.Y)/(float v1.Y)))
        else
            None

let segmentIntersectionList (seg: Segment) (intersectors: Segment list) : (int * SegmentIntersect) list =
    let ltOne a = a - EPSILON < 1.0
    let gtZero a = a + EPSILON > 0.0
    intersectors 
    |> List.indexed
    |> List.choose (fun (i, ints) ->
        match segmentsIntersect seg ints with
        | None -> None
        | Some ints -> Some (i, ints))
    |> List.map (function
        | i, Overlap (a,b) ->
            let a, b = 
                if a < b then
                    max a 0., min b 1.
                else
                    max b 0., min a 1.
            (i, Overlap (a, b))
        | iints -> iints)
    |> List.filter (function
        | _, Point (a,_,b) -> gtZero a && ltOne a && gtZero b && ltOne b
        | _, Overlap (a,b) -> ltOne a && gtZero b)

type IncidenceType = Touch | Cross
type Decomposition =
    | DecPoint of float * IncidenceType
    | DecOverlap of float * float

/// Decompose a segment according to its intersection with a simple polygon
/// This assumes that the simplePolygon's segments are given in order, i.e. that
/// a segment's end point is always the next segment's starting point 
let segmentDecomposition (seg: Segment) (simplePolygon: Segment list) : Decomposition list =
    // Get the ordered list of intersections
    let intersections = segmentIntersectionList seg simplePolygon
    if List.isEmpty intersections then
        []
    else
    // Convert to Decomposition
    // Convert repeated points to CrossPoint / TouchPoint
    let lastDir  =
        // Compute the last Point's direction
        // This should only be done if the first Point is an exit-point, i.e.
        // intersects at 0
        let segArr = Array.ofList simplePolygon
        let findPoint =
            intersections
            |> List.tryFind (fun (_, ints) ->
                match ints with
                | Point _ -> true
                | _ -> false)
        match findPoint with
        | None -> CW // No points, doesn't matter
        | Some (_, Overlap _) -> CW // Will never happen
        | Some (firstIdx, Point (_, _, b)) ->
        if not (isZero b) then CW // first Point is not an exit, doesn't matter
        else
        let segv = intVectorOfSegment seg
        let rec getLastDir i =
            let i = if i = 0 then segArr.Length-1 else i - 1
            if i = firstIdx then CW // Failsafe
            else
            let holeSegv = intVectorOfSegment segArr.[i]
            let d = intVectorDeterminant segv holeSegv
            if d = 0 then
                getLastDir i
            else
                _intDeterminantToDirection d
        getLastDir firstIdx
    let (decompositionsRev, _) = 
        List.fold (fun (acc, lastDir) cur ->
            match cur with
            | Point (acur, curDir, bcur) ->
                match isZero bcur, isOne bcur with
                | false, false ->
                    // In the middle of a hole segment
                    (DecPoint (acur, Cross)::acc, curDir)
                | _, true ->
                    // At the end of a hole segment
                    (acc, curDir)
                | true, _ -> 
                    // At the beginning of a hole segment
                    let typ = if curDir = lastDir then Cross else Touch
                    (DecPoint (acur, typ)::acc, curDir)
            | Overlap (a1, a2) ->
                (DecOverlap (a1, a2)::acc, lastDir)
            ) ([], lastDir) (List.map snd intersections)
    decompositionsRev
    |> List.rev 
    // Sort the decompositions by intersection point
    |> List.sortBy (function
        | DecPoint (a, _) -> (a, a)
        | DecOverlap (a,b) -> (a, b))


/// A structure for precomputed set of points in the hole
type HolePoints = {
    Arr: bool[,]
    Dx: int
    Dy: int
}

/// Return the set of points of the hole of the problem
// let getHolePoints (problem: Problem) =
//     let holeSegs = holeSegments problem
//     let cmin, cmax = holeBoundingBox problem
//     let arr = Array2D.init (cmax.X - cmin.X + 1) (cmax.Y - cmin.Y + 1) (fun _ _ -> false)
//     let markPoint (x: int, y: int) =
//         arr.[x - cmin.X,y - cmin.Y] <- true
//     for y in cmin.Y .. cmax.Y do
//         let seg = (Coord (cmin.X, y), Coord (cmax.X, y))
//         let lastCross = ref -1.
//         let inHole = ref false
//         segmentDecomposition seg holeSegs
//         |> List.iter (function
//             | TouchPoint a ->
//                 if !inHole then
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
