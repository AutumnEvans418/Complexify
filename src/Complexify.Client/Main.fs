module Complexify.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Parser
open Runner
/// The Elmish application's model.
type Model =
    {
        mathInput:string
        compCount: int
        mathOutput:string
        mathCalculation:string
    }

let getComplex s c = 
    let tree = getTree s |> complexifyRepeat c
    let compValue = tree |> displayTree
    let result = tree |> Runner.doMath |> displayResult
    (compValue, result)

let initModel =
    let comp, v = getComplex "2+2" 3
    {
        compCount = 3
        mathInput = "2+2"
        mathOutput = comp
        mathCalculation = v
    }

/// The Elmish application's update messages.
type Message =
    | InputChanged of string * int

let update message model =
    match message with
    | InputChanged (s, i) ->
        let result, v = getComplex s i
        { model with mathOutput = result; compCount = i; mathInput = s; mathCalculation = v }, Cmd.none

/// Connects the routing system to the Elmish application.
//let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main()
        .MathInput(model.mathInput, fun s -> dispatch (InputChanged (s, model.compCount)))
        .MathOutput(model.mathOutput)
        .CompCount(model.compCount |> string, fun s -> dispatch (InputChanged (model.mathInput, int(s))))
        .EvalResult(model.mathCalculation)
        .Elt()

let view model dispatch =
    homePage model dispatch

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        //|> Program.withRouter router
