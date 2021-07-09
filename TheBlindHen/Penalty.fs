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