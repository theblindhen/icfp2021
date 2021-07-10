module Penalty

open Model
open Geometry

type PenaltyFn = Problem -> Figure -> float

/// Return the allowed ranges for each edge in the problem
let problemEdgeLengthSqRanges (problem: Problem) =
    figureSegments problem.Figure
    |> List.map (fun segment ->
           let len = segmentLengthSq segment
           ((float len) * (1. - (float problem.Epsilon)/1000000.),
            (float len) * (1. + (float problem.Epsilon)/1000000.)))
    |> Array.ofList


/// Return the sum of how much each edge's sq-length is outside the allowed range
let penaltyEdgeLengthSqSum (problem: Problem) =
    let edgeSqRanges = problemEdgeLengthSqRanges problem
    fun (fig: Figure) ->
        figureSegments fig
        |> List.map (segmentLengthSq >> float)
        |> List.mapi (fun i lensq ->
            let (min, max) = edgeSqRanges.[i]
            if lensq >= min && lensq <= max then
                0.0
            else
                if lensq < min then
                    min - lensq
                else
                    lensq - max)
        |> List.sum

/// Return the sum of each edge's ratio to the allowed range
let penaltyEdgeRatioSum (problem: Problem) =
    let segmentSqRanges = problemEdgeLengthSqRanges problem
    fun (fig: Figure) ->
        figureSegments fig
        |> List.map (segmentLengthSq >> float)
        |> List.mapi (fun i lensq ->
            let (min, max) = segmentSqRanges.[i]
            if lensq >= min && lensq <= max then
                0.0
            else
                if lensq < min then
                    min / lensq
                else
                    lensq / max)
        |> List.sum

// /// Return the number of edge crossing between the figure and the problem hole 
// /// TODO: Implement memoization for efficiency
// let penaltyEdgeCrossings (problem: Problem) =
//     let holeEdges = holeEdges problem
//     fun (fig: Figure) ->
//         figureEdges fig
//         |> List.sumBy (fun edge ->
//             if List.exists (fun e -> edgesIntersects e edge) holeEdges then
//                 1.0
//             else
//                 0.0)

/// TODO: replace with getHolePoints
let isCoordInsideHole holeSegments coord =
    let segment = (Coord (-1,-1), coord)
    let decoms = segmentDecomposition segment holeSegments
    let isOne a = abs (a - 1.) < EPSILON
    let rec iter isInside =
        function
        | CrossPoint a :: decomTl -> if isOne a then true else iter (not isInside) decomTl
        | TouchPoint a :: decomTl -> if isOne a then true else iter isInside decomTl
        | Aligned (_,b) :: decomTl -> if isOne b then true else iter isInside decomTl
        | [] -> isInside
    iter false decoms

/// returns the ratio of the sement that is outside the hole
let segmentOutsideHole holeSegments (a,b) =
    let decoms = segmentDecomposition (a,b) holeSegments
    let rec iter (isInside, acc, last) =
        function
        | CrossPoint a :: decomTl -> 
            let acc = if isInside then acc else acc + a - last
            iter (not isInside, (if isInside then acc else acc + a - last), a) decomTl
        | TouchPoint _ :: decomTl -> iter (isInside, acc, last) decomTl
        | Aligned (a,b) :: decomTl -> 
            let accUpd = if isInside then acc else acc + a - last
            let lastUpd = if isInside then b else last
            iter (not isInside, acc, lastUpd) decomTl            
        | [] -> if isInside then acc else acc + 1.0 - last
    iter (isCoordInsideHole holeSegments a, 0.0, 0.0) decoms

let penaltyOutsideHole (problem: Problem) =
    let hole = holeSegments problem
    figureSegments problem.Figure
    |> List.map (fun seg -> (seg, segmentOutsideHole hole seg))
    |> List.sumBy (fun (seg, ratio) -> ratio * (sqrt (float (segmentLengthSq seg))))

let rasterizeHole (problem: Problem) =
    // TODO: bitmap, not full color
    // TODO: pick the right resolution
    let info = SkiaSharp.SKImageInfo(400, 400)
    use surface = SkiaSharp.SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SkiaSharp.SKColors.White)
    use paint =
        new SkiaSharp.SKPaint(
            IsAntialias = false,
            Color = SkiaSharp.SKColors.Black,
            StrokeJoin = SkiaSharp.SKStrokeJoin.Round, // TODO: right choice?
            StrokeCap = SkiaSharp.SKStrokeCap.Round) // TODO: right choice?
    let path = new SkiaSharp.SKPath()
    let points = problem.Hole
    path.MoveTo(float32 points.[0].X, float32 points.[0].Y)
    for i in 1 .. points.Length - 1 do
        path.LineTo(float32 points.[i].X, float32 points.[i].Y)
    path.Close()
    // TODO: Make sure we fill with black. Is that the default behavior?
    // https://skia.org/docs/user/api/skpath_overview/
    canvas.DrawPath(path, paint)
    // TODO: conservative fill, maybe retracting by 1 pixel
    // TODO: return a bitmap of some sort
