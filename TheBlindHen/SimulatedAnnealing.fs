module SimulatedAnnealing

open System

open Model

type GetNeighbor<'state> = 'state -> (string * 'state) option

let private calculateMedian
    (eStart: float)
    (getRandomNeighborEnergy: unit -> float option)
    =
    printfn "SimulatedAnnealing: initial energy %f" eStart
    // Take a number of samples and find the median of the energy differences in
    // cases where the neighbors are better.
    let samples =
        [| 1 .. 1000 |]
        |> Array.choose (fun _ ->
            getRandomNeighborEnergy ())
        |> Array.map (fun e -> eStart - e)
        |> Array.filter (fun delta -> delta > 0.0)
        |> Array.sort
    printfn "SimulatedAnnealing: found %d samples" (Array.length samples)
    if Array.isEmpty samples then
        printfn "SimulatedAnnealing: FOUND NO INITIAL NEIGHBORS!"
        printfn "SimulatedAnnealing: using 1%% of %f" eStart
        0.001 * eStart
    else
        // Find the median of the samples.
        let median = samples.[Array.length samples / 2]
        printfn "SimulatedAnnealing: found median %f" median
        median

/// Based on https://en.wikipedia.org/wiki/Simulated_annealing.
/// After passing unit, you get a function that computes the next state, or None
/// if we reached the max number of iterations.
let generalSimulatedAnnealing
    (temperatureSchedule: float -> float)
    (energyFunction: 'state -> float)
    (getRandomNeighbor: GetNeighbor<'state>)
    (iterations: int)
    () =
        let rnd = Util.getRandom ()
        let i = ref 0
        let medianRef = ref None
        // TODO: in this way, we end up calling the energy function on the same
        // state many times. Only return something if the state changed.
        fun state ->
            let e = energyFunction state
            let median =
                match !medianRef with
                | Some t -> t
                | None ->
                    let getRandomNeighborEnergy () =
                        getRandomNeighbor state |> Option.map (snd >> energyFunction)
                    let median = calculateMedian e getRandomNeighborEnergy
                    medianRef := Some median
                    median
            let acceptanceFunction e e' temperature =
                if e' < e then
                    1.0
                else
                    //printfn "T = %f; %f - %f = %f; P = %f" temperature e' e (e' - e)
                    //    (1.0 + Math.Log(1.0 + (e' - e) / median))
                    //exp (- (e' - e) / temperature)
                    temperature / (1.0 + Math.Pow(Math.Log(1.0 + (e' - e) / median), 2.0))
            if !i < iterations then
                i := !i + 1
                let temperature = temperatureSchedule (float !i / float iterations)
                //printfn "i = %d, T = %f" !i temperature
                match getRandomNeighbor state with
                | Some (desc, state') ->
                    let e' = energyFunction state'
                    if e' <= 0.0 then
                        printfn "Solution found after %d iterations" !i
                        (Some (ChoseNeighbor desc, state'), e') // solution found
                    else if acceptanceFunction e e' temperature >= rnd.NextDouble() then
                        //printfn "  Accepting candidate solution"
                        (Some (ChoseNeighbor desc, state'), e') // step to next state
                    else
                        (Some (RejectedNeighbor desc, state), e) // stay in previous state
                | None -> (Some (NowhereToGo, state), e)
            else
                (None, e) // no more iterations

let temperatureScheduleSimple (ratio: float) =
    //0.2 * exp (- 10.0 * ratio)
    //0.2 / (1.0 + 30.0 * ratio)
    0.2 * exp (- 10.0 * ratio)

let simpleSimulatedAnnealing
    (energyFunction: 'state -> float)
    (getRandomNeighbor: GetNeighbor<'state>)
    (iterations: int) =
        generalSimulatedAnnealing temperatureScheduleSimple
            energyFunction getRandomNeighbor iterations
