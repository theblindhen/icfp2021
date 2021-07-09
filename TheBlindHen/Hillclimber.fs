module Hillclimber

let cons x y = x :: y


/// Generates an infinite sequence of solutions using hill-climbing
/// to visit neighbors with the lowest cost (or the current solution,
/// if no neighbors are better)
///
/// The neighbors function may be non-deterministic; if the neighbors function
/// is deterministic, then repeats in the solution sequence implies that a local
/// minimum has been reached
let hillClimber neighbors cost =
    Seq.unfold (fun solution -> 
        solution 
        |> neighbors 
        |> cons solution
        |> List.minBy cost 
        |> (fun x -> Some (x, x)))


/// Optimize the given solution using hill-climbing with the given
/// neighbor and cost function; stops after the given number of iterations
/// or a solution is encountered with a non-positive cost, whichever comes first
///
/// Maximum number of iterations must be positive
let runHillClimber neighbors cost maxIterations solution =
    Seq.append (Seq.singleton solution) (hillClimber neighbors cost solution)
    |> Seq.pairwise
    |> Seq.take maxIterations
    |> Seq.takeWhile (fun (prevSol, _) -> cost prevSol > 0.0)
    |> Seq.last
    |> snd
