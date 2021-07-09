module GUI

open System

let problemGlobalVar: Model.Problem option ref = ref None

let figurePenalty (problem: Model.Problem) =
    Penalty.penaltyEdgeLengthSqSum problem

let stepSolver (problem: Model.Problem) =
    let rnd = System.Random (int System.DateTime.Now.Ticks)
    let neighbors = Neighbors.translateRandomCoord rnd
    Hillclimber.step neighbors (figurePenalty problem)

module Counter =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Avalonia.Controls.Shapes

    type State = {
        Problem: Model.Problem
        CurrentFigure: Model.Figure
    }

    let init (problem: Model.Problem) =
        let mangledFigure = Model.copyFigure problem.Figure
        //mangledFigure.Vertices.[1] <- Model.Coord(12, 15)
        //mangledFigure.Vertices.[2] <- Model.Coord(25, 20)
        {
            Problem = problem
            CurrentFigure = mangledFigure
        }

    type Msg = Increment | Decrement | Reset

    let update (msg: Msg) (state: State) : State =
        match msg with
        // TODO: pre-compute a stepper
        | Increment -> { state with CurrentFigure = stepSolver state.Problem state.CurrentFigure }
        | Decrement -> state
        | Reset -> state
    
    let view (state: State) (dispatch) =
        let scale = 2.0
        DockPanel.create [
            DockPanel.children [
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch Reset)
                    Button.content "reset"
                ]                
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch Decrement)
                    Button.content "-"
                ]
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch Increment)
                    Button.content "+"
                ]
                Canvas.create [
                    Canvas.background "#2c3e50"
                    Canvas.children (
                        (
                            let vs = state.CurrentFigure.Vertices
                            state.CurrentFigure.Edges
                            |> Array.toList
                            |> List.map (fun (s,t) ->
                                Line.create [
                                    Line.startPoint (float vs.[s].X * scale, float vs.[s].Y * scale)
                                    Line.endPoint (float vs.[t].X * scale, float vs.[t].Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#e74c3c"
                                ] :> Avalonia.FuncUI.Types.IView
                            )
                        ) @
                        (
                            Model.holeSegments state.Problem
                            |> List.map (fun (s,t) ->
                                Line.create [
                                    Line.startPoint (float s.X * scale, float s.Y * scale)
                                    Line.endPoint (float t.X * scale, float t.Y * scale)
                                    Line.strokeThickness 2.0
                                    Line.stroke "#000000"
                                ] :> Avalonia.FuncUI.Types.IView
                            )
                        )
                    )
                ]       
            ]
        ]       

open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts

type MainWindow() as this =
    inherit HostWindow()
    do
        base.Title <- "TestUI"
        base.Width <- 400.0
        base.Height <- 400.0
        
        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let problem = Option.get !problemGlobalVar

        Elmish.Program.mkSimple Counter.init Counter.update Counter.view
        |> Program.withHost this
        |> Program.runWith problem

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

let showGui problem =
        problemGlobalVar := Some problem 
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime([||]) // TODO: how to parse args
