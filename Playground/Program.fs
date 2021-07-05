// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Text.Json

[<Struct>]
type Point = {
    X: int
    Y: int
}

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    let p = { X = 11; Y = 22 }
    let pJSON = JsonSerializer.Serialize p
    printfn "pJSON = %s" pJSON
    let p' : Point = JsonSerializer.Deserialize pJSON
    printfn "p' =\n%A" p'
    0 // return an integer exit code