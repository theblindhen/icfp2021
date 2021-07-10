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

let stepSolver (problem: Model.Problem) =
    let rnd = System.Random (int System.DateTime.Now.Ticks)
    let getNeighbor = Neighbors.balancedCollectionOfNeighbors
    let penalties = Penalty.figurePenalties problem
    let penaltySum fig = List.sum (penalties fig)
    let step = SimulatedAnnealing.simpleSimulatedAnnealing penaltySum getNeighbor 100_000 rnd ()
    fun figure ->
        let result = Option.defaultValue figure (step figure)
        // TODO: this is just debug printing
        let resPenalties = penalties result
        let spenalties =
            resPenalties
            |> List.map (fun p -> sprintf "%.2f" p) 
            |> String.concat " + "
        printfn $"penalty = {spenalties} = {List.sum(resPenalties)}"
        result

let solve (problemPath: string) (problemNo: int) =
    let problem = Model.parseFile $"{problemPath}/{problemNo}.problem" // TODO: call from GUI
    //let solutionPath = Some ($"{problemPath}/{problemNo}-solutions/")
    let stepper = stepSolver problem
    let outsideHolePenalty = Penalty.outsideHolePenalty problem // TODO: repeat
    let rec run i figure =
        if i = 100_000 then
            ()
        else
            let figure = stepper figure
            printfn "%7d %f" i (outsideHolePenalty figure)
            run (i + 1) figure
    run 0 problem.Figure
