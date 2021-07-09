// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main args =
    match args with
    | [| "parse"; inputFile |] ->
        let problem = Model.parseFile inputFile
        printfn "%A" problem
        printfn "The same problem, pretending to be a solution"
        let mockSolution = { Model.SolutionVertices = problem.Figure.Vertices }
        printfn "%s" (Model.deparseSolution mockSolution)
        0 // return an integer exit code
    | [| "gui" |] ->
        GUI.showGui ()
    | _ ->
        printfn "Usage: see source code!"
        1
