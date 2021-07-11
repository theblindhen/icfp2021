module SimulatedAnnealing

open System

/// Based on https://en.wikipedia.org/wiki/Simulated_annealing.
/// After passing unit, you get a function that computes the next state, or None
/// if we reached the max number of iterations.
let generalSimulatedAnnealing
    (acceptanceFunction: float -> float -> float -> float)
    (temperatureSchedule: float -> float)
    (energyFunction: 'state -> float)
    (getRandomNeighbor: Random -> 'state -> 'state)
    (iterations: int)
    () =
        let rnd = Util.getRandom ()
        let i = ref 0
        // TODO: in this way, we end up calling the energy function on the same
        // state many times. Only return something if the state changed.
        fun state ->
            let e = energyFunction state
            if !i < iterations then
                i := !i + 1
                let temperature = temperatureSchedule (float !i / float iterations)
                let state' = getRandomNeighbor rnd state
                let e' = energyFunction state'
                if e' <= 0.0 then
                    (Some state', e') // solution found
                else if acceptanceFunction e e' temperature >= rnd.NextDouble() then
                    (Some state', e') // step to next state
                else
                    (Some state, e) // stay in previous state
            else
                (None, e) // no more iterations

let acceptanceFunctionSimple e e' temperature =
    if e' < e then
        1.0
    else
        temperature

let temperatureScheduleSimple (ratio: float) =
    0.2 * exp (- 10.0 * ratio)

let acceptanceFunctionStandard e e' temperature =
    if e' < e then
        1.0
    else
        exp (- (e' - e) / temperature)

let simpleSimulatedAnnealing
    (energyFunction: 'state -> float)
    (getRandomNeighbor: Random -> 'state -> 'state)
    (iterations: int) =
        generalSimulatedAnnealing acceptanceFunctionSimple temperatureScheduleSimple
            energyFunction getRandomNeighbor iterations
