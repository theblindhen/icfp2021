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
    let getNeighbor = Neighbors.balancedCollectionOfNeighbors problem
    Solver.simulatedAnnealingStepper problem getNeighbor 100_000

let solve (problem: Model.Problem) (writeSolution: Model.Figure -> unit) =
    let stepper = stepSolver problem
    let mutable moveDescs = []
    let rec run i figure =
        let (result, figure, penalty) = stepper figure
        //printfn "  %7d %f" i penalty
        if Util.verbose () && i % 1_000 = 0 && i > 0 then
            printfn $"Iterations {i-1_000}-{i}: (penalty: {penalty})"
            moveDescs
            |> List.countBy id
            |> List.iter (fun (desc, c) ->
                printfn $"  {desc} occured {c} times")
            moveDescs <- []
        match result with
        | Model.StopCriteria ->
            printfn "No more iterations left. Penalty %f" penalty
            if Penalty.isValid problem figure then
                writeSolution figure
        | desc when penalty = 0.0 ->
            printfn "Problem solved perfectly! OMG!"
            writeSolution figure
        | desc ->
            //printfn $"Iteration %d{i}: {desc}"
            moveDescs <- desc::moveDescs
            run (i + 1) figure
    run 0 problem.Figure
