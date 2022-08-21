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
/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home

/// The Elmish application's model.
type Model =
    {
        page: Page
        mathInput:string
        compCount: int
        mathOutput:string
        error: string option
    }

let getComplex s c = getTree s |> complexifyRepeat c |> displayTree

let initModel =
    {
        page = Home
        compCount = 3
        mathInput = "2+2"
        mathOutput = getComplex "2+2" 3
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | InputChanged of string * int
    | Error of exn
    | ClearError

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    | InputChanged (s, i) ->
        let result = getComplex s i
        { model with mathOutput = result }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home()
        .MathInput(model.mathInput, fun s -> dispatch (InputChanged (s, model.compCount)))
        .MathOutput(model.mathOutput)
        .CompCount(model.compCount, fun s -> dispatch (InputChanged (model.mathInput, int(s))))
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model Home "Home"
        })
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
