module Penalty

open Model
open Geometry

type PenaltyFn = Problem -> Figure -> float

let problemEdgeLengthSqRanges (problem: Problem) =
    figureEdges problem.Figure
    |> List.map (fun edge ->
           let len = edgeLengthSq edge
           ((float len) * (1. - (float problem.Epsilon)/1000000.),
            (float len) * (1. + (float problem.Epsilon)/1000000.)))
    |> Array.ofList


// Return the sum of how much each edge's sq-length is outside the allowed range
let penaltyEdgeLengthSqSum (problem: Problem) =
    let edgeSqRanges = problemEdgeLengthSqRanges problem
    fun (fig: Figure) ->
        figureEdges fig
        |> List.map (edgeLengthSq >> float)
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

// Return the sum of each edge's ratio to the allowed range
let penaltyEdgeRatioSum (problem: Problem) =
    let edgeSqRanges = problemEdgeLengthSqRanges problem
    fun (fig: Figure) ->
        figureEdges fig
        |> List.map (edgeLengthSq >> float)
        |> List.mapi (fun i lensq ->
            let (min, max) = edgeSqRanges.[i]
            if lensq >= min && lensq <= max then
                0.0
            else
                if lensq < min then
                    min / lensq
                else
                    lensq / max)
        |> List.sum