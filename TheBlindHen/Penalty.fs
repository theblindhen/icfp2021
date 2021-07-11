module Penalty

open Util
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

/// Returns the diff btw an edgeLenSq and the allowed lenSq range for the problem.
/// If it is too short, the number is negative, if too long, positive.
/// WARNING: Remember to call abs on the result for use in a penalty!
let edgeLengthExcessSq (problem: Problem) =
    let edgeSqRanges = problemEdgeLengthSqRanges problem
    fun (edgeIdx: int) (edge: Segment) ->
        let (min, max) = edgeSqRanges.[edgeIdx]
        let lensq = float (segmentLengthSq edge)
        if lensq >= min && lensq <= max then
            0.0
        else
            if lensq < min then
                lensq - min //Negative: too short
            else
                lensq - max //Positive: too long

/// Return the sum of how much each edge's sq-length is outside the allowed range
let penaltyEdgeLengthSqSum (problem: Problem) =
    let edgeLengthExcessSq = edgeLengthExcessSq problem
    fun (fig: Figure) ->
        figureSegments fig
        |> List.mapi edgeLengthExcessSq
        |> List.sumBy abs

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
let isCoordInsideHole holeSegments =
    memoize (fun coord ->
        let segment = (Coord (-1,-1), coord)
        let decoms = segmentDecomposition segment holeSegments
        let rec iter isInside =
            function
            | DecPoint (a, typ) :: decomTl ->
                if isOne a then
                    true
                else
                    iter (if typ = Cross then not isInside else isInside) decomTl
            | DecOverlap (_,b) :: decomTl ->
                if isOne b then
                    true
                else
                    iter isInside decomTl
            | [] -> isInside
        iter false decoms)

let segmentStartsInside holeSegments =
    memoize (fun (sStart: Coord, sEnd: Coord) ->
        let extSeg = (Coord (sStart.X - 1000 * (sEnd.X - sStart.X), 
                             sStart.Y - 1000 * (sEnd.Y - sStart.Y)),
                      sStart)
        let decoms = segmentDecomposition extSeg holeSegments
        let rec iter isInside =
            function
            | DecPoint (a, typ) :: decomTl ->
                if isOne a then
                    isInside
                else
                    iter (if typ = Cross then not isInside else isInside) decomTl
            | DecOverlap (_,b) :: decomTl ->
                iter isInside decomTl
            | [] -> isInside
        iter false decoms)

/// returns the ratio of the sement that is outside the hole
let segmentOutsideHole holeSegments =
    let isInside = segmentStartsInside holeSegments
    memoize (fun seg ->
        let decoms = segmentDecomposition seg holeSegments
        let rec iter (isInside, acc, lastCross) =
            let accUpd acc a =
                if isInside then acc else acc + (max 0. (a - lastCross))
            function
            | DecPoint (a, typ) :: decomTl -> 
                let typUpd = if typ = Cross then not isInside else isInside
                iter (typUpd, accUpd acc a, a) decomTl
            | DecOverlap (a,b) :: decomTl -> 
                iter (isInside, accUpd acc a, b) decomTl            
            | [] -> accUpd acc 1.0
        iter (isInside seg, 0.0, 0.0) decoms
        )

let penaltyEdgeRatioOutside (problem: Problem) =
    let segmentOutsideHole = segmentOutsideHole (holeSegments problem)
    fun fig ->
        figureSegments fig
        |> List.map (fun seg -> (seg, segmentOutsideHole seg))
        |> List.sumBy (fun (seg, ratio) -> ratio * float (segmentLengthSq seg))

let outsideHoleEndpointPenalty (problem: Problem): Figure -> float =
    let isNotInHole = memoize(SkiaUtil.isInHole problem.Hole >> not)
    let vertexPenalty = (fun (c: Coord) ->
        let minDist = Array.minBy (fun (hc: Coord) -> segmentLengthSq (c, hc))
                                  problem.Hole
        segmentLengthSq (c, minDist))
    fun figure ->
        figure.Vertices
        |> Array.filter isNotInHole
        |> Array.sumBy vertexPenalty
        |> float

let outsideHoleSegmentPenaltySkia (problem: Problem): Figure -> float =
    let isInHole = SkiaUtil.isInHole problem.Hole //TODO: Memoize?
    let holeSegments = Model.holeSegments problem
    fun figure ->
        figure.Edges
        |> Array.sumBy (fun (s, t) ->
            let sc, tc = figure.Vertices.[s], figure.Vertices.[t]
            if isInHole sc && isInHole tc && segmentIntersectionList (sc, tc) holeSegments <> [] then
                segmentLengthSq (sc, tc) |> float
            else 0.0)

let dislikes (problem: Problem) (figure : Figure) : int =
    problem.Hole
    |> Array.sumBy (fun (c: Coord) ->
        (segmentLengthSq (c, Array.minBy (fun (hc: Coord) -> segmentLengthSq (c, hc)) figure.Vertices)))

let dislikesPenalty (problem: Problem): Figure -> float =
    fun figure -> 
        dislikes problem figure |> float     

let isValid (problem: Problem): Figure -> bool =
    let penalties = [ penaltyEdgeLengthSqSum
                      outsideHoleEndpointPenalty 
                      penaltyEdgeRatioOutside
                    ] |> List.map (fun partPenalty -> partPenalty problem)
    fun figure ->
        List.sumBy (fun penalty -> penalty figure) penalties = 0.0

let figurePenalties (problem: Problem): Figure -> float list =
    let penalties = [ 1_000_000_000.0, penaltyEdgeLengthSqSum
                      1_000_000_000.0, outsideHoleEndpointPenalty 
                      //1_000_000_000.0, outsideHoleSegmentPenalty Skia
                      1_000_000_000.0, penaltyEdgeRatioOutside
                      1.0, dislikesPenalty
                    ] |> List.map (fun (weight, partPenalty) -> (weight, partPenalty problem))
    fun figure ->
        List.map (fun (weight, penalty) -> weight * penalty figure) penalties

let figurePenalty (problem: Problem): Figure -> float =
    let penalties = figurePenalties problem
    fun figure ->
        let pens = penalties figure
        List.iteri (fun i p ->
            if p < -EPSILON then
                printfn $"ERROR: The {i}'th penalty returned negative!!") pens
        List.sum pens

let figurePenaltiesToString (problem: Problem): Figure -> string =
    let penalties = figurePenalties problem
    fun figure ->
        let resPenalties = penalties figure
        let spenalties =
            resPenalties
            |> List.map (fun p -> sprintf "%.1f" p) 
            |> String.concat " + "
        sprintf $"penalty = {spenalties} = %.1f{List.sum(resPenalties)}"

/// Return a badness for each vertex in the figure, based on the penalties it
/// incurs.
let vertexBadness (problem: Problem) =
    let OUTSIDE_BADNESS = 1000.
    let isInHole = memoize (SkiaUtil.isInHole problem.Hole)
    let edgeLengthExcessSq = edgeLengthExcessSq problem
    let edgeMap = incidentEdgeMap problem.Figure
    fun (fig: Figure) ->
        let edgeLenPens =
            { 0..Array.length fig.Edges-1 }
            |> Seq.map (fun edgeIdx ->
                let edge = segmentOfVertexIdxPair fig fig.Edges.[edgeIdx]
                edgeLengthExcessSq edgeIdx edge)
            |> Array.ofSeq
        fig.Vertices
        |> Array.mapi (fun vertexIdx vertex ->
            let edgeContribution =
                Map.find vertexIdx edgeMap
                |> List.sumBy (fun i -> edgeLenPens.[i])
            let isOutsideContribution =
                if isInHole vertex then 0.0 else OUTSIDE_BADNESS
            edgeContribution + isOutsideContribution)