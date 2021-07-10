module FitInHole

open System

let rnd = Random(int(DateTime.Now.Ticks))

let holeBBPenalty (minCorner: Model.Coord, maxCorner: Model.Coord) (figure: Model.Figure) =
    let square x = x * x
    figure.Vertices
    |> Array.sumBy (fun xy ->
        square (max 0 (minCorner.X - xy.X)) +
        square (max 0 (xy.X - maxCorner.X)) +
        square (max 0 (minCorner.Y - xy.Y)) +
        square (max 0 (xy.Y - maxCorner.Y)))
    |> float

let figurePenalty (problem: Model.Problem) =
    let outsideHolePenalty = Penalty.outsideHolePenalty problem
    fun figure ->
        Penalty.penaltyEdgeLengthSqSum problem figure +
        outsideHolePenalty figure

let stepSolver (problem: Model.Problem) =
    let rnd = System.Random (int System.DateTime.Now.Ticks)
    let getNeighbor = Neighbors.balancedCollectionOfNeighbors
    let penalty = figurePenalty problem
    let outsideHolePenalty = Penalty.outsideHolePenalty problem
    let step = SimulatedAnnealing.simpleSimulatedAnnealing penalty getNeighbor 100_000 rnd ()
    fun figure ->
        let result = Option.defaultValue figure (step figure)
        // TODO: this is just debug printing
        printfn "penalty = %f + %f"
            (Penalty.penaltyEdgeLengthSqSum problem result)
            (outsideHolePenalty result)
        result
