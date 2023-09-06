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
    type Model = { error: string option }
    type Msg = ClearError | GoHome | Error of string
    let init initialCmd =
        { error = None }, Cmd.batch initialCmd
    let update msg model =
        model, Cmd.Empty

    let view (model: Model) dispatch =
        let class' (className: string) element (children: ReactElement list) =
            element [prop.className className; prop.children children]
        let classP' (className: string) element (props: IReactProperty list) =
            element (prop.className className::props)

        match model.error with
        | Some error ->
            class' "errorMsg" Html.div [
                Html.div [Html.text error]
                Html.button [prop.text "OK"; prop.onClick (thunk1 dispatch ClearError)]
                Html.button [prop.text "Start over"; prop.onClick (thunk1 dispatch GoHome)]
                ]
        | None ->
            Html.div [
                prop.className "homePage"
                prop.children [
                    Html.div [
                        Html.h1 "Shining Sword Cage Fight!"
                        Html.h3 "For Dungeon Fantasy RPG and GURPS"
                        ]
                    class' "main" Html.div [
                        classP' "commandInput" Html.form [
                            prop.children [
                                Html.text "Command: "
                                Html.input [prop.placeholder "Peshkali vs. N Orcs"]
                                Html.button [prop.text "OK"; prop.type'.submit]
                                ]
                            prop.onSubmit(fun e -> e.preventDefault())
                            ]
                        class' "fightSetup" Html.div [
                            class' "specificQuantity" Html.div [
                                Html.input [prop.placeholder "Filter"]
                                Html.div [
                                    Html.button [prop.text "+"]
                                    Html.button [prop.text "-"]
                                    Html.text "3 peshkalis"
                                    ]
                                ]
                            Html.text "vs."
                            class' "calibrated" Html.div [
                                Html.input [prop.placeholder "Filter"]
                                Html.div [
                                    Html.button [prop.text "+"]
                                    Html.button [prop.text "-"]
                                    Html.text "N orcs"
                                    class' "calibrationRange" Html.span [
                                        Html.input [prop.type'.number; prop.placeholder "50"]
                                        Html.text "% to "
                                        Html.input [prop.type'.number; prop.placeholder "80"]
                                        Html.text "%"
                                        ]
                                    ]
                                ]
                            Html.button [prop.text "Execute"]
                            ]
                        class' "statistics" Html.div [
                            Html.div "3 peshkalis wins 50-80% of the time against 14-17 orcs"
                            ]
                        class' "fightLog" Html.div [
                            Html.div "Peshkali hits orc 1, blahblahblah"
                            Html.button [prop.text "<<"]
                            Html.button [prop.text "<"]
                            Html.button [prop.text ">"]
                            Html.button [prop.text ">>"]
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
