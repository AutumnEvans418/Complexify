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
    }

let getComplex s c = getTree s |> complexifyRepeat c |> displayTree

let initModel =
    {
        compCount = 3
        mathInput = "2+2"
        mathOutput = getComplex "2+2" 3
    }

/// The Elmish application's update messages.
type Message =
    | InputChanged of string * int

let update message model =
    match message with
    | InputChanged (s, i) ->
        let result = getComplex s i
        { model with mathOutput = result; compCount = i; mathInput = s; }, Cmd.none

/// Connects the routing system to the Elmish application.
//let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main()
        .MathInput(model.mathInput, fun s -> dispatch (InputChanged (s, model.compCount)))
        .MathOutput(model.mathOutput)
        .CompCount(model.compCount, fun s -> dispatch (InputChanged (model.mathInput, int(s))))
        .Elt()

let view model dispatch =
    homePage model dispatch

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        //|> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
