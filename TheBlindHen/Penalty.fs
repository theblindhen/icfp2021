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
    let (=~) x y = abs (x - y) < EPSILON
    let rec iter isInside =
        function
        | DecPoint (a, typ) :: decomTl ->
            if a =~ 1. then
                true
            else
                iter (if typ = Cross then not isInside else isInside) decomTl
        | DecOverlap (_,b) :: decomTl ->
            if b =~ 1. then
                true
            else
                iter isInside decomTl
        | [] -> isInside
    iter false decoms

/// returns the ratio of the sement that is outside the hole
let segmentOutsideHole holeSegments (a,b) =
    let decoms = segmentDecomposition (a,b) holeSegments
    let rec iter (isInside, acc, lastCross) =
        function
        | DecPoint (a, Cross) :: decomTl -> 
            let accUpd = if isInside then acc else acc + a - lastCross
            let lastUpd = a
            iter (not isInside, accUpd, lastUpd) decomTl
        | DecPoint (_, Touch) :: decomTl -> iter (isInside, acc, lastCross) decomTl
        | DecOverlap (a,b) :: decomTl -> 
            let accUpd = if isInside then acc else acc + a - lastCross
            let lastUpd = b
            iter (isInside, accUpd, lastUpd) decomTl            
        | [] -> if isInside then acc else acc + 1.0 - lastCross
    iter (isCoordInsideHole holeSegments a, 0.0, 0.0) decoms

let penaltyOutsideHole (problem: Problem) =
    let hole = holeSegments problem
    figureSegments problem.Figure
    |> List.map (fun seg -> (seg, segmentOutsideHole hole seg))
    |> List.sumBy (fun (seg, ratio) -> ratio * (sqrt (float (segmentLengthSq seg))))

let holeAsPath (problem: Problem) =
    let path = new SkiaSharp.SKPath()
    let points = problem.Hole
    path.MoveTo(float32 points.[0].X, float32 points.[0].Y)
    for i in 1 .. points.Length - 1 do
        path.LineTo(float32 points.[i].X, float32 points.[i].Y)
    path.Close()
    path

let outsideHolePenalty (problem: Problem): Figure -> float =
    let hole = holeAsPath problem
    fun figure ->
        figure.Vertices
        |> Array.filter (fun (c: Coord) -> not (hole.Contains(float32 c.X, float32 c.Y)))
        |> Array.sumBy (fun (c: Coord) ->
                segmentLengthSq (c, Array.minBy (fun (hc: Coord) -> segmentLengthSq (c, hc)) problem.Hole))
        |> float