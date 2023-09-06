module UI.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core
open Domain.Character.DungeonFantasy

importSideEffects "../sass/main.sass"

module App =
    open Domain.Character
    open Domain.Character.Universal
    type Page =
        | Home
        | Generate of Chargen.View.Model
    type Model = { current: Page; currentUrl: string option; error: string option; roster: CharacterSheet array; graveyard: CharacterSheet array }
    type Msg = ClearError | GoHome | Error of string
    let init initialCmd =
        { current = Page.Home; currentUrl = None; error = None; roster = LocalStorage.PCs.read(); graveyard = LocalStorage.Graveyard.read() }, Cmd.batch initialCmd
    let update msg model =
        model, Cmd.Empty

    let view (model: Model) dispatch =
        let class' element (className: string) (children: ReactElement list) =
            element [prop.className className; prop.children children]

        match model.current with
        | _ when model.error.IsSome ->
            class' Html.div "errorMsg" [
                Html.div [Html.text model.error.Value]
                Html.button [prop.text "OK"; prop.onClick (thunk1 dispatch ClearError)]
                Html.button [prop.text "Start over"; prop.onClick (thunk1 dispatch GoHome)]
                ]
        | _ ->
            Html.div [
                prop.className "homePage"
                prop.children [
                    Html.div "Shining Sword Cage Fight!"
                    Html.div [
                        prop.className "footer"
                        prop.children [
                            Html.a [prop.href "https://www.flaticon.com/free-icons/sword"; prop.text "Sword icon created by pongsakornRed - Flaticon"]
                            ]
                        ]
                    ]
               ]

module Url =
    open Chargen.View
    open App
    module Parse =
        open Browser.Types
        open Packrat
        let locationParser (rootActivePattern: ParseRule<_>) (loc: Location) =
            let (|Root|_|) = rootActivePattern
            match ParseArgs.Init loc.hash with
            | Str "#" (Root(v, End)) -> v
            | _ -> []

        //let (|Page|_|) = function
        //    | Str "chargen/df/swash" ctx ->
        //        let model' = Chargen.View.init (Some { Constraints.fresh with professionPreference = Some Swashbuckler })
        //        let cmd = [
        //            Open (Page.Generate model', None)
        //            ChargenMsg (SetRuleset (Chargen.View.DungeonFantasy))
        //            ]
        //        Some(cmd, ctx)
        //    | Str "chargen/df" ctx ->
        //        let model' = Chargen.View.init None
        //        let cmd = [
        //            Open (Page.Generate model', None)
        //            ChargenMsg (SetRuleset (Chargen.View.DungeonFantasy))
        //            ]
        //        Some(cmd, ctx)
        //    | Str "chargen" ctx ->
        //        let model' = Chargen.View.init None
        //        let cmd = [
        //            Open (Page.Generate model', None)
        //            ]
        //        Some(cmd, ctx)
        //    | Str "resume/" (Any(id, ctx)) ->
        //        Some([ResumePlay id], ctx)
        //    | Str "/" ctx | Str "" (End as ctx) ->
        //        Some([GoHome], ctx)
        //    | _ -> None

        // let page = locationParser (|Page|_|)
    let parse loc =
        // let parsed = Parse.page loc
        // parsed |> List.map Cmd.ofMsg
        [] // this goes to init ([])
    let update msg model = // update via URL navigation (from parse output), as opposed to directly via dispatch
        model, Cmd.Empty

open App
open Elmish
open Elmish.Navigation
open Elmish.HMR

Program.mkProgram init update view
|> Program.withSubscription(fun m -> [
    [], fun dispatch ->
            Browser.Dom.window.onerror <-
                fun msg ->
                    if msg.ToString().Contains "SocketProtocolError" = false then
                        dispatch (sprintf "Error: %A" msg |> Error)
                        // Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
            React.createDisposable (fun () -> Browser.Dom.window.onerror <- ignore)
        ])
|> Program.toNavigable Url.parse Url.update
|> Program.withReactBatched "feliz-app"
|> Program.run
