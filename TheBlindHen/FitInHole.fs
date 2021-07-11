module FitInHole

open System

let holeBBPenalty (minCorner: Model.Coord, maxCorner: Model.Coord) (figure: Model.Figure) =
    let square x = x * x
    figure.Vertices
    |> Array.sumBy (fun xy ->
        square (max 0 (minCorner.X - xy.X)) +
        square (max 0 (xy.X - maxCorner.X)) +
        square (max 0 (minCorner.Y - xy.Y)) +
        square (max 0 (xy.Y - maxCorner.Y)))
    |> float

let stepSolver (problem: Model.Problem) =
    let getNeighbor = Neighbors.balancedCollectionOfNeighbors
    let penalties = Penalty.figurePenalties problem
    let penaltySum fig = List.sum (penalties fig)
    let step = SimulatedAnnealing.simpleSimulatedAnnealing penaltySum getNeighbor 100_000 ()
    step

let solve (problem: Model.Problem) (writeSolution: Model.Figure -> unit) =
    let stepper = stepSolver problem
    let rec run i figure =
        let (result, penalty) = stepper figure
        //printfn "  %7d %f" i penalty
        match result with
        | None ->
            printfn "No more iterations left. Penalty %f" penalty
        | Some figure when penalty = 0.0 ->
            printfn "Problem solved! OMG!"
            writeSolution figure
        | Some figure ->
            run (i + 1) figure
    run 0 problem.Figure
