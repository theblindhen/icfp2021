module Hillclimber

let step neighbors cost state =
    let cons x y = x :: y
    state
    |> neighbors 
    |> cons state
    |> List.minBy cost 

/// Generates an infinite sequence of states using hill-climbing
/// to visit neighbors with the lowest cost (or the current state,
/// if no neighbors are better)
///
/// The neighbors function may be non-deterministic; if the neighbors function
/// is deterministic, then repeats in the state sequence implies that a local
/// minimum has been reached
let hillClimber neighbors cost =
    Seq.unfold (fun state -> 
        let state = step neighbors cost state 
        Some (state, state))

/// Optimize the given state using hill-climbing with the given
/// neighbor and cost function; stops after the given number of iterations
/// or a state is encountered with a non-positive cost, whichever comes first
///
/// Maximum number of iterations must be positive
let runHillClimber neighbors cost maxIterations state =
    Seq.append (Seq.singleton state) (hillClimber neighbors cost state)
    |> Seq.pairwise
    |> Seq.take maxIterations
    |> Seq.takeWhile (fun (prevSol, _) -> cost prevSol > 0.0)
    // |> Seq.map (fun (prevSol, state) ->
    //                 listener prevSol;
    //                 (prevSol, state))
    |> Seq.tryLast
    |> Option.map snd
    |> Option.defaultValue state
