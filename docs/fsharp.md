# Running F# code

* To run the REPL for quick experiments, use `dotnet fsi`.

* To run the project using VSCode's debugger, press F5, optionally after
  selecting a target by clicking the Debug-button in the left-hand-side palette.
  When using a Debug target, execution will stop at breakpoints that you insert
  in the code.

* To run the project in the terminal, type something like:

```
> dotnet run -c Release --project Playground/Playground.fsproj
```
  

## F# Projects

To make a new console project (stand-alone executable) in the solution, go to the solution's base folder and type:

```
> dotnet new console -lang "F#"  -n "HelloWorld"
```

To make a new class library project, instead run: 

```
> dotnet new classlib -lang "F#"  -n "MyLib"
```

Go to the Solution Explorer (you will need to install `vscode-solution-explorer`
extension), right-click on the solution and select `Add existing project`.
Select the newly created project.

Open `launch.json` to set up launch targets.

To use a library project in a console project, we need to set up project
references: click the F# button in the left-hand-side palette. Expand your
console application, and its `Project References`. Right-click and select "Add
project references". Select your library project.
You can now use the library in any of the files of the console project using
`Open <library-name>`. Rebuilding of the library happens automatically.

## Graph libraries

- [Kts.AStar.Smartrak](https://www.nuget.org/packages/Kts.AStar.Smartrak/). A* only. Appears to have everything: good data structures, tests, arbitrary node type, implicit graphs, cancellable async.
- [astar-search](https://www.nuget.org/packages/astar-search/). A* only. Implemented in F# and supports implicit graphs over any comparable node type. Uses functional maps and sets internally, which may be slow. Sorts a `list` rather than using a priority queue. The implementation is so small we can just copy it and modify it as we like.
- [AStar.net](https://www.nuget.org/packages/AStar.net). A* only. [Supports implicit graphs](https://www.fuget.org/packages/AStar.net/1.1.0/lib/netstandard2.0/AStar.net.dll/AStarNet/INodeMap%601) but forces the use of [their own `Node` type](https://www.fuget.org/packages/AStar.net/1.1.0/lib/netstandard2.0/AStar.net.dll/AStarNet/Node%601). No tests.
- [FSharp.FGL](https://github.com/CSBiology/FSharp.FGL). Persistent (i.e. immutable) graphs. Seems esoteric but may be useful if the game board itself is a graph, like in the 2017 punting game. Performance is tuned to some specific algorithms but may be totally off for others.

## Parsing libraries

TODO

## Cross platform GUI libraries

- [Avalonia FuncUI](https://github.com/fsprojects/Avalonia.FuncUI). Allows writing an Elm-style UI (MVU). The documentation for the MVU parts appears to be the source code, and it doesn't cover all of Avalonia's vast API. How big are the dependencies?
- [Fabulous for Xamarin.Forms](https://github.com/fsprojects/Fabulous). Allows writing an Elm-style UI (MVU). Looks production-ready and well documented. How big are the dependencies?
- [Elmish.WPF](https://github.com/elmish/Elmish.WPF). Advertises MVU but actually requires the UI to be statically described in an XML-based format.

## Machine learning libraries
TODO

## Game boards, including hypothetical game boards

These are often bit sets or tuples of bit sets. For hypothetical boards, we're always torn between cloning the whole board at every search step or representing the board as an asymptotically better persistent data structure that has 10-100x more constant overhead. Is there a way to get the best of both worlds?

## Unit testing

TODO

## Language features to be aware of

- String interpolation: `$"Hello, {region}"` and `$"Hello, %s{region}"`.

- [Discriminated union fields](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions)
  can have names:
  ```fsharp
  type Shape = Rectangle of width: float * height: float
  ...
  let getShapeWidth shape =
    match shape with
    | Rectangle(width = w) -> w
  ```.

- [Struct tuples](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/tuples):
  `struct(1, "foo")` has type `struct (int * string)`.

- [Struct records](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records):
 Put `[<Struct>]` on record types or [discriminated union types](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions#struct-discriminated-unions).
