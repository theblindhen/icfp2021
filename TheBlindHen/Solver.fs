module Solver

open System
open Model

type Stepper = Figure -> MoveDesc * Figure * float

let simulatedAnnealingStepper (problem: Problem) getNeighbor iterations =
    let penalties = Penalty.figurePenalties problem
    let penaltySum fig = List.sum (penalties fig)
    let step = SimulatedAnnealing.simpleSimulatedAnnealing penaltySum getNeighbor iterations ()
    step

let runSolver (stepper: Stepper) figure =
    let rec run i figure =
        match stepper figure with
        | (Model.StopCriteria, _, _) -> figure
        | (_, figure, 0.0) -> figure
        | (_, figure, _) -> run (i+1) figure
    run 0 figure
