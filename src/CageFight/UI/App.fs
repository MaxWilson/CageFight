module UI.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Domain

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core

importSideEffects "../sass/main.sass"

module App =
    type Page =
        | Home
        | Editing of name:string
    type Opposition =
        | Calibrate of string option * int option * int option
        | Specific of (int * string) list
    type FightSetup = {
        sideA: (int * string) list
        sideB: Opposition
        }
    type Model = { error: string option; page: Page; fightSetup: FightSetup; database : MonsterDatabase }
    type Msg =
        | ClearError | Error of string
        | ChangeFight of (FightSetup -> FightSetup)
        | SetPage of Page
    let update msg model =
        match msg with
        | ChangeFight f -> { model with fightSetup = f model.fightSetup }, Cmd.Empty
        | SetPage page -> { model with page = page }, Cmd.Empty
        | _ ->
            model, Cmd.Empty
    let init () =
        let db =
            ["Peshkali"; "Orc"; "Galdurnaut"; "Skeleton"]
            |> List.map (fun name -> Creature.create name)
            |> List.fold (fun db monster -> MonsterDatabase.add monster db) MonsterDatabase.fresh
        let fight = {
            sideA = [3, "Peshkali"; 1, "Galdurnaut"]
            sideB = Calibrate(Some "Orc", None, None)
            }
        { error = None; page = Home; fightSetup = fight; database = db }, Cmd.Empty

    let view (model: Model) dispatch =
        let class' (className: string) element (children: ReactElement list) =
            element [prop.className className; prop.children children]
        let classP' (className: string) element (props: IReactProperty list) =
            element (prop.className className::props)

        match model.error, model.page with
        | Some error, _ ->
            class' "errorMsg" Html.div [
                Html.div [Html.text error]
                Html.button [prop.text "OK"; prop.onClick (thunk1 dispatch ClearError)]
                Html.button [prop.text "Start over"; prop.onClick (thunk1 dispatch (SetPage Home))]
                ]
        | None, Editing name ->
            Html.div [
                Html.input [prop.valueOrDefault name]
                Html.button [prop.text "Cancel"; prop.onClick (fun _ -> dispatch (SetPage Home))]
                Html.button [prop.text "OK"]
                ]
        | None, Home ->
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
                            let editButton (name: string) =
                                classP' "editButton" Html.button [prop.text "Edit"; prop.onClick(fun _ -> dispatch (SetPage (Editing name)))]
                            let monsterPicker monsterDetails =
                                Html.div [
                                    Html.input [prop.placeholder "Monster name"]
                                    Html.button [prop.text "New"]
                                    monsterDetails
                                    ]
                            class' "specificQuantity" Html.div [
                                Html.div [
                                    for quantity, name in model.fightSetup.sideA do
                                        let changeQuantity delta (f: FightSetup) =
                                            { f with sideA = f.sideA |> List.map (function (quantity, name') when name = name' -> (quantity + delta |> max 1, name) | otherwise -> otherwise) }
                                        Html.div [
                                            Html.button [prop.text "+"; prop.onClick (fun _ -> dispatch (ChangeFight (changeQuantity +1)))]
                                            Html.button [prop.text "-"; prop.onClick (fun _ -> dispatch (ChangeFight (changeQuantity -1)))]
                                            Html.text $"{quantity} {name}s"
                                            editButton name
                                            ]
                                    ]
                                |> monsterPicker
                                ]
                            Html.text "vs."
                            class' "calibrated" Html.div [
                                let wrapInDiv (element: ReactElement) = Html.div [element]
                                let onClick msg =
                                    prop.onClick (fun _ -> dispatch (ChangeFight msg))
                                let changeMode fight =
                                    { fight
                                        with
                                        sideB =
                                            match fight.sideB with
                                            | Specific ((quantity, name)::_) -> Calibrate(Some name, None, None)
                                            | Specific _ -> Calibrate(None, None, None)
                                            | Calibrate(Some name, _, _) -> Specific [1, name]
                                            | _ -> Specific []
                                        }
                                monsterPicker <| React.fragment [
                                    match model.fightSetup.sideB with
                                    | Specific sideB ->
                                        Html.button [prop.text "Specific number"; onClick changeMode]
                                            |> wrapInDiv
                                        for quantity, name in sideB do
                                            let changeQuantity delta (f: FightSetup) =
                                                let changeSideB = function
                                                    | Specific lst ->
                                                        lst
                                                        |> List.map (function (quantity, name') when name = name' -> (quantity + delta |> max 1, name) | otherwise -> otherwise)
                                                        |> Specific
                                                    | otherwise -> otherwise
                                                { f with sideB = f.sideB |> changeSideB }
                                            Html.div [
                                                Html.button [prop.text "+"; onClick (changeQuantity +1)]
                                                Html.button [prop.text "-"; onClick (changeQuantity -1)]
                                                Html.text $"{quantity} {name}s"
                                                editButton name
                                                ]
                                    | Calibrate(name, min, max) ->
                                        Html.button [prop.text "Find optimal quantity"; onClick changeMode]
                                            |> wrapInDiv
                                        match name with
                                        | Some name ->
                                            Html.div [
                                                Html.text $"N {name}s"
                                                editButton name
                                                Html.text "should lose"
                                                class' "calibrationRange" Html.span [
                                                    Html.input [prop.type'.number; prop.placeholder (defaultArg min 50 |> toString); match min with Some min -> prop.valueOrDefault min | None -> ()]
                                                    Html.text "% to "
                                                    Html.input [prop.type'.number; prop.placeholder (defaultArg min 80 |> toString); match max with Some max -> prop.valueOrDefault max | None -> ()]
                                                    Html.text "% of the time"
                                                    ]
                                                ]
                                        | None -> ()
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
        () // this goes to init ()
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
