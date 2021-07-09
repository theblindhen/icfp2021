// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let inputFile = argv.[1]
    let problem = Model.parseFile inputFile
    printfn "%A" problem
    0 // return an integer exit code