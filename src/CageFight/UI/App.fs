module UI.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Domain
open Domain.Random
open Domain.Random.Parser
// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core
open UI.FightExecution
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
    type 't Awaitable =
        | NotStarted
        | InProgress
        | Completed of 't
    type Model = {
        error: string option
        page: Page
        fightSetup: FightSetup
        database : MonsterDatabase
        execution: Awaitable<FightSetup * FightResult>
        }
    type Side = SideA | SideB
    type Msg =
        | ClearError | Error of string
        | ChangeFight of (FightSetup -> FightSetup)
        | Clear of Side
        | Upsert of Creature
        | SetPage of Page
        | Fight of (FightSetup * FightResult) Awaitable
    let update msg model =
        match msg with
        | ChangeFight f -> { model with fightSetup = f model.fightSetup }, Cmd.Empty
        | SetPage page -> { model with page = page }, Cmd.Empty
        | Error err -> { model with error = Some err }, Cmd.Empty
        | ClearError -> { model with error = None }, Cmd.Empty
        | Clear side ->
            let clearSide = function
                | SideA -> { model.fightSetup with sideA = [] }
                | SideB -> { model.fightSetup with sideB = Calibrate(None, None, None) }
            { model with fightSetup = clearSide side }, Cmd.Empty
        | Upsert creature ->
            if creature.name |> String.isntWhitespace then
                let db = MonsterDatabase.add creature model.database
                LocalStorage.Catalog.write db.catalog
                { model with database = db }, Cmd.Empty
            else model, Cmd.Empty
        | Fight v -> { model with execution = v }, Cmd.Empty

    let init () =
        let db =
            { MonsterDatabase.fresh with catalog = LocalStorage.Catalog.read() }
        let fight = {
            sideA = [3, "Peshkali"; 1, "Slugbeast"]
            sideB = Calibrate(Some "Orc", None, None)
            }
        { error = None; page = Home; fightSetup = fight; database = db; execution = NotStarted }, Cmd.Empty

    let beginFights (model: Model) dispatch =
        if model.execution = InProgress then
            ()
        else
            let g = System.Guid.NewGuid()
            Fight InProgress |> dispatch
            async {
                do! Async.Sleep 100 // force async to yield long enough for the busy animation to show up--I'm not sure why but it doesn't happen sometimes otherwise
                match model.fightSetup.sideB with
                | _ when (model.fightSetup.sideA |> List.sumBy fst) = 0 ->
                    Fight NotStarted |> dispatch
                    dispatch (Error "You have to pick monsters first")
                | Calibrate(None, _, _) ->
                    Fight NotStarted |> dispatch
                    dispatch (Error "You have to pick monsters first")
                | Calibrate(Some name, min, max) ->
                    let min = (defaultArg min 50 |> float) / 100.
                    let max = (defaultArg max 90 |> float) / 100.
                    match calibrate model.database.catalog
                            model.fightSetup.sideA
                            (name, min, max) with
                    | minQuantity, maxQuantity, Some sampleMaxFight ->
                        Completed(model.fightSetup, CalibratedResult(minQuantity, maxQuantity, sampleMaxFight)) |> Fight |> dispatch
                    | v ->
                        Fight NotStarted |> dispatch
                        Error "Failed to find a number of monsters that would satisfy those constraints. Try a wider range like 20% to 100%" |> dispatch
                | Specific(sideB) ->
                    let fightResult = specificFight model.database.catalog model.fightSetup.sideA sideB
                    (model.fightSetup, SpecificResult fightResult)
                        |> Completed
                        |> Fight
                        |> dispatch
                } |> Async.StartImmediate |> ignore

    let [<ReactComponent>] monsterPicker (db: MonsterDatabase, noMonstersSelectedYet) (clickLabel: string, onClick, side, dispatch) (monsterDetails: ReactElement) =
        let namePrefix, update = React.useState ""
        Html.div [
            Html.input [prop.placeholder "Monster name"; prop.valueOrDefault namePrefix; prop.onChange update]
            classP' "newButton" Html.button [prop.text "New"; prop.onClick(thunk1 dispatch (SetPage (Editing "")))]
            classP' "clearButton" Html.button [prop.text "Clear"; prop.onClick(thunk1 dispatch (Clear side))]
            monsterDetails
            if namePrefix.Length > 0 || noMonstersSelectedYet then
                let matchingNames = db.catalog.Keys |> Seq.filter (fun name -> name.StartsWith(namePrefix, System.StringComparison.InvariantCultureIgnoreCase)) |> List.ofSeq
                for name in matchingNames |> List.take (min 10 matchingNames.Length) do
                    Html.div [
                        Html.button [prop.text clickLabel; prop.onClick(fun _ -> onClick name)]
                        Html.text name
                        ]
            ]

    let labeled (label: string) (inner: ReactElement) =
        Html.div [
            Html.text label
            inner
            ]
    let editData<'t> (propType: IReactProperty, render: 't -> string, parser: string -> 't option) (label:string) (hint: 't) (value: 't option, update: 't option -> unit) =
        let txt, updateTxt = React.useState (match value with Some v -> render v | None -> "")
        labeled label <| Html.input [
            prop.valueOrDefault txt
            prop.placeholder (render hint)
            propType
            if propType = prop.type'.number then
                prop.max 99
            prop.onChange updateTxt
            prop.onBlur (fun _ ->
                let v = parser txt
                update v
                match v with
                | Some v -> updateTxt (render v)
                | None -> updateTxt "")
            ]
    let editDropdown<'t> (render: 't -> string, parser: string -> 't option) (label:string) (hint: 't) (value: 't option, options, update: 't option -> unit) =
        labeled label <| Html.select [
            match value with
            | Some value ->
                prop.valueOrDefault (render value)
            | None ->
                prop.placeholder (render hint)
            prop.children [
                for option in options do
                    Html.option [prop.text (render option); prop.value (render option)]
                ]
            prop.onChange (parser >> update)
            ]
    let editDamage (label:string) (stats: Creature) update =
        let value = stats.Damage
        let render = function
        | Explicit r -> toString r
        | Swing 0 -> "sw"
        | Thrust 0 -> "thr"
        | Swing v -> $"sw%+d{v}"
        | Thrust v -> $"thr%+d{v}"
        let txt, updateTxt = React.useState (match value with Some v -> render v | None -> $"")
        class' "editDamage" Html.div [
            Html.text label
            Html.input [
                prop.valueOrDefault txt
                prop.placeholder (toString stats.Damage_)
                prop.onChange updateTxt
                prop.onBlur (fun _ ->
                    match Packrat.ParseArgs.Init txt with
                    | Domain.Parser.DamageOverall((dmg, dtype), Packrat.End) ->
                        let stats' =
                            { stats with Damage = Some dmg; DamageType = dtype |> Option.orElse stats.DamageType }
                        update stats'
                        updateTxt (dmg |> render)
                    | _ ->
                        let stats' =
                            { stats with Damage = None }
                        update stats'
                        updateTxt ""
                    )
                ]
            match stats.Damage with
            | None | Some (Explicit _) -> () // if it's placeholdered or explicitly set then showing again would be redundant
            | _ -> Html.text $"({stats.Damage_ |> toString})"
            ]
    let [<ReactComponent>] editView (name: string) (db: MonsterDatabase) dispatch =
        let stats = (db.catalog |> Map.tryFind name |> Option.defaultValue (Creature.create name))
        let stats, update = React.useState stats
        let editString = editData<string>(prop.type'.text, toString, (fun (txt: string) -> if String.isntWhitespace txt then Some txt else None))
        let editNumber = editData(prop.type'.number, toString, (fun (input: string) -> match System.Int32.TryParse input with true, n -> Some n | _ -> None))
        let editDecimalNumber = editData(prop.type'.number, (fun v -> $"%.2f{v}"), (fun (input: string) -> match System.Double.TryParse input with true, n -> Some n | _ -> None))
        let editDamageType = editDropdown(toString, (fun (input: string) -> match Packrat.ParseArgs.Init input with Domain.Parser.DamageType (r, Packrat.End) -> Some r | _ -> None))
        let editBool label (value: bool, update) =
            labeled label <| Html.input [
                prop.type'.checkbox
                prop.isChecked value
                prop.onCheckedChange update
                ]
        class' "editView" Html.div [
            editString "Name" "" (Some stats.name, (fun txt -> { stats with name = defaultArg txt "" } |> update))
            editString "Pluralized" (stats.name + "s") (stats.pluralName, (fun txt -> { stats with pluralName = txt } |> update))
            editNumber "ST" stats.ST_ (stats.ST, (fun n -> { stats with ST = n } |> update))
            editNumber "DX" stats.DX_ (stats.DX, (fun n -> { stats with DX = n } |> update))
            editNumber "IQ" stats.IQ_ (stats.IQ, (fun n -> { stats with IQ = n } |> update))
            editNumber "HT" stats.HT_ (stats.HT, (fun n -> { stats with HT = n } |> update))
            editNumber "HP" stats.HP_ (stats.HP, (fun n -> { stats with HP = n } |> update))
            editDecimalNumber "Speed" stats.Speed_ (stats.Speed, (fun n -> { stats with Speed = n } |> update))
            editBool "Weapon Master" (stats.WeaponMaster, (fun b -> { stats with WeaponMaster = b } |> update))
            editNumber "Weapon Skill" 10 (stats.WeaponSkill, (fun n -> { stats with WeaponSkill = n } |> update))
            editDamage "Damage" stats update
            editDamageType "Damage type" DamageType.Other (stats.DamageType, [Crushing; Cutting; Piercing; Impaling; Other], (fun v -> { stats with DamageType = v } |> update))

            Html.button [prop.text "Cancel"; prop.onClick (fun _ -> dispatch (SetPage Home))]
            Html.button [prop.text "OK"; prop.onClick (fun _ -> dispatch (Upsert stats); dispatch (SetPage Home))]
            ]


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
                Html.button [prop.text "Start over"; prop.onClick (fun _ -> dispatch ClearError; dispatch (SetPage Home))]
                ]
        | None, Home when model.execution = InProgress ->
            class' "slideFromTop" Html.div [
                Html.div "Executing..."
                class' "busy" Html.div [
                    for _ in 1..10 do
                        class' "wave" Html.div []
                    ]
                ]
        | None, Editing name -> editView name model.database dispatch
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
                            let editLink (quantity: int option) (name: string) =
                                // for aesthetic reasons, we don't want quantity to be part of the link, so it's a separate HTML element
                                let numberTxt, txt =
                                    let creature = model.database.catalog[name]
                                    match quantity with
                                    | Some 1 -> Html.text "1", creature.name
                                    | Some q -> Html.text (toString q), creature.PluralName_
                                    | None -> Html.text "N", creature.PluralName_
                                React.fragment [
                                    numberTxt
                                    classP' "editLink" Html.a [prop.text txt; prop.onClick(fun _ -> dispatch (SetPage (Editing name)))]
                                    ]
                            class' "specificQuantity" Html.div [
                                let changeQuantity name delta (f: FightSetup) =
                                    { f with
                                        sideA =
                                            f.sideA
                                            |> List.map (function (quantity, name') when name = name' -> (quantity + delta, name) | otherwise -> otherwise)
                                            |> List.filter (fun (quantity, _) -> quantity > 0)
                                            }
                                let addToSideA name fightSetup =
                                    if fightSetup.sideA |> List.exists (fun (_, name') -> name = name') then
                                        changeQuantity name +1 fightSetup
                                    else
                                        { fightSetup with
                                            sideA = fightSetup.sideA@[1, name]
                                            }
                                monsterPicker (model.database, model.fightSetup.sideA.IsEmpty) <|
                                    ("Add", addToSideA >> ChangeFight >> dispatch, SideA, dispatch) <|
                                        Html.div [
                                            match model.fightSetup.sideA with
                                            | [] -> Html.text "No creatures selected"
                                            | sideA ->
                                                for quantity, name in sideA do
                                                    Html.div [
                                                        Html.button [prop.text "+"; prop.onClick (fun _ -> dispatch (ChangeFight (changeQuantity name +1)))]
                                                        Html.button [prop.text "-"; prop.onClick (fun _ -> dispatch (ChangeFight (changeQuantity name -1)))]
                                                        editLink (Some quantity) name
                                                        ]
                                            ]
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
                                match model.fightSetup.sideB with
                                | Specific sideB ->
                                    let changeQuantity name delta (f: FightSetup) =
                                        let changeSideB = function
                                            | Specific lst ->
                                                lst
                                                |> List.map (function (quantity, name') when name = name' -> (quantity + delta |> max 1, name) | otherwise -> otherwise)
                                                |> Specific
                                            | otherwise -> otherwise
                                        { f with sideB = f.sideB |> changeSideB }
                                    let addToSideB name fightSetup =
                                        if sideB |> List.exists (fun (_, name') -> name = name') then
                                            changeQuantity name +1 fightSetup
                                        else
                                            { fightSetup with
                                                sideB = Specific(sideB@[1, name])
                                                }

                                    monsterPicker (model.database, sideB.IsEmpty) ("Add", addToSideB >> ChangeFight >> dispatch, SideB, dispatch) <| React.fragment [
                                        Html.button [prop.text "Specific number"; onClick changeMode]
                                            |> wrapInDiv
                                        match sideB with
                                        | [] -> Html.text "No creatures selected"
                                        | sideB ->
                                            for quantity, name in sideB do
                                                Html.div [
                                                    Html.button [prop.text "+"; onClick (changeQuantity name +1)]
                                                    Html.button [prop.text "-"; onClick (changeQuantity name -1)]
                                                    editLink (Some quantity) name
                                                    ]
                                        ]
                                | Calibrate(name, min, max) ->
                                    let setSideB name fightSetup =
                                        { fightSetup with sideB = Calibrate(Some name, min, max)
                                            }
                                    monsterPicker (model.database, name.IsNone) ("Set", setSideB >> ChangeFight >> dispatch, SideB, dispatch) <| React.fragment [
                                        Html.button [prop.text "Find optimal quantity"; onClick changeMode]
                                            |> wrapInDiv
                                        match name with
                                        | Some name ->
                                            Html.div [
                                                editLink None name
                                                Html.text "should lose"
                                                class' "calibrationRange" Html.span [
                                                    let changeMin (txt: string) =
                                                        let v = match System.Int32.TryParse txt with true, v -> Some v | _ -> None
                                                        ChangeFight (fun fight -> { fight with sideB = Calibrate(Some name, v, max) }) |> dispatch
                                                    let changeMax (txt: string) =
                                                        let v = match System.Int32.TryParse txt with true, v -> Some v | _ -> None
                                                        ChangeFight (fun fight -> { fight with sideB = Calibrate(Some name, min, v) }) |> dispatch
                                                    Html.input [
                                                        prop.type'.number; prop.placeholder (defaultArg min 50 |> toString)
                                                        prop.onChange changeMin; prop.max 99
                                                        match min with Some min -> prop.valueOrDefault min | None -> ()
                                                        ]
                                                    Html.text "% to "
                                                    Html.input [
                                                        prop.type'.number; prop.placeholder (defaultArg max 90 |> toString)
                                                        prop.onChange changeMax; prop.max 99
                                                        match max with Some max -> prop.valueOrDefault max | None -> ()
                                                        ]
                                                    Html.text "% of the time"
                                                    ]
                                                ]
                                        | None -> Html.div "No creatures selected"
                                        ]
                                ]
                            ]
                        Html.button [prop.text "Execute"; prop.onClick (thunk2 beginFights model dispatch)]
                        match model.execution with
                        | NotStarted | InProgress -> ()
                        | Completed (setup, result) ->
                            let db = model.database
                            let teamToTxt team =
                                team
                                |> List.map (
                                    function
                                    | (1, name) -> name
                                    | (n, name) -> $"{n} {db.catalog[name].PluralName_}")
                                |> String.oxfordJoin
                            match result with
                            | SpecificResult (combat, victors) ->
                                match victors.victors with
                                | [1] ->
                                    Html.div $"Team One wins!"
                                | [2] ->
                                    Html.div $"Team Two wins!"
                                | [] ->
                                    Html.div $"Everybody dies!"
                                | _ ->
                                    Html.div $"Stalemate!"
                            | CalibratedResult(minQuantity, maxQuantity, sampleCombat) ->
                                let name, min, max = match setup.sideB with Calibrate(Some name, min, max) -> name, min, max | _ -> shouldntHappen()
                                let min = defaultArg min 50
                                let max = defaultArg max 90
                                class' "statistics" Html.div [
                                    let quantityDescription =
                                        match minQuantity, maxQuantity with
                                        | Some n, Some m when n = m -> $"{n}"
                                        | Some n, None -> $"{n} or more"
                                        | None, Some n -> $"{n} or fewer"
                                        | Some n, Some m -> $"{n} to {m}"
                                        | None, None -> "an unknown number of"
                                    Html.div $"{setup.sideA |> teamToTxt} wins {min}%%-{max}%% of the time against {quantityDescription} {db.catalog[name].PluralName_}"
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
                    System.Diagnostics.Debugger.Break()
                    if msg.ToString().Contains "SocketProtocolError" = false then
                        dispatch (sprintf "Error: %A" msg |> Error)
                        // Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
            React.createDisposable (fun () -> Browser.Dom.window.onerror <- ignore)
        ])
|> Program.toNavigable Url.parse Url.update
|> Program.withReactBatched "feliz-app"
|> Program.run
