module UI.FightExecution
open Domain
open Domain.Random

type Status = OK | Stunned | Prone | Unconscious | Dead
type CombatantId = int * string
type Combatant = {
    personalName: string
    number: int
    team: int
    stats: Creature
    damageTaken: int
    statusMods: Status list
    retreatUsed: bool
    blockUsed: bool
    parriesUsed: int
    }
    with
    member this.CurrentHP_ = this.stats.HP_ - this.damageTaken
    member this.Id : CombatantId = this.team, this.personalName
    static member fresh (team, name, number, stats: Creature) =
        {   team = team
            personalName = name
            number = number
            stats = stats
            damageTaken = 0
            statusMods = []
            retreatUsed = false
            blockUsed = false
            parriesUsed = 0
            }
type Combat = {
    combatants: Map<CombatantId, Combatant>
    }
type Ids =
    { attacker: CombatantId; target: CombatantId }
    with
    member this.Attacker_ = snd this.attacker
    member this.AttackerTeam_ = fst this.attacker
    member this.Target_ = snd this.target
    member this.TargetTeam_ = fst this.target
type DefenseType = Parry | Block | Dodge
type DefenseDetails = { defense: DefenseType; targetRetreated: bool }

[<AutoOpen>]
module CombatEvents =
    type Event =
        | Hit of Ids * DefenseDetails * injury:int * Status list * string
        | SuccessfulDefense of Ids * DefenseDetails * string
        | Miss of Ids * string
        | FallUnconscious of CombatantId * string
        | Unstun of CombatantId * string
        | StandUp of CombatantId * string
        | Info of CombatantId * msg: string * rollInfo: string
        | NewRound of int
    let update msg model =
        let updateCombatant id (f: Combatant -> Combatant) model =
            { model with
                combatants =
                    model.combatants |> Map.change id (function | Some c -> Some (f c) | None -> None)
                }
        let consumeDefense (id: CombatantId) (defense: DefenseDetails) =
            updateCombatant id (fun c ->
                { c with
                    retreatUsed = c.retreatUsed || defense.targetRetreated
                    blockUsed = c.blockUsed || defense.defense = Block
                    parriesUsed = c.parriesUsed + (if defense.defense = Parry then 1 else 0)
                    })
        let resetDefenses (id: CombatantId) =
            updateCombatant id (fun c ->
                { c with
                    retreatUsed = false
                    blockUsed = false
                    parriesUsed = 0
                    })
        let takeDamage (id: CombatantId) amount conditions =
            updateCombatant id (fun c ->
                { c with
                    damageTaken = c.damageTaken + amount
                    statusMods = List.distinct (c.statusMods @ conditions)
                    })
        match msg with
        | Hit (ids, defense, injury, statusImpact, rollDetails) ->
            model |> resetDefenses ids.attacker
                  |> consumeDefense ids.target defense
                  |> takeDamage ids.target injury statusImpact
        | SuccessfulDefense(ids, defense, rollDetails) ->
            model |> resetDefenses ids.attacker
                  |> consumeDefense ids.target defense
        | Miss (ids, rollDetails) ->
            model |> resetDefenses ids.attacker
        | FallUnconscious(id, rollDetails) ->
            model |> takeDamage id 0 [Unconscious]
        | Unstun(id, rollDetails) ->
            model |> updateCombatant id (fun c ->
                { c with statusMods = c.statusMods |> List.filter ((<>) Stunned) })
        | StandUp(id, rollDetails) ->
            model |> updateCombatant id (fun c ->
                { c with statusMods = c.statusMods |> List.filter ((<>) Prone) })
        | Info _ | NewRound _ -> model
type CombatLog = (Event option * Combat) list
type FightResult =
    | CalibratedResult of lower:int option * upper:int option * sample:CombatLog
    | SpecificResult of CombatLog * {| victors: int list |}

let tryFindTarget (combat: Combat) (attacker: Combatant) =
    let betweenInclusive (min, max) x = min <= x && x <= max
    let potentialTargets =
        combat.combatants.Values
        |> Seq.filter (fun c -> c.team <> attacker.team)
        |> Seq.filter (fun c -> c.statusMods |> List.forall (fun s -> not (s = Dead || s = Unconscious)))
        // We don't want to overkill damage, so we put targets that might fall unconscious by themselves
        // fairly low in priority, although we also want to damage vulnerable targets while they're vulnerable
        // so we put stunned and prone targets at high priority.
        // prefer targets that are stunned but not yet at -HP,
        // then targets that are prone but not yet at -HP,
        // then targets that are below 1/3 HP but not yet at 0 HP,
        // then targets at or below 0 HP
        // then anyone still alive (ordered by statblock name and number because why not, and it makes readouts more predictable)
        |> Seq.sortBy(fun c ->
            (c.statusMods |> List.exists ((=) Stunned))
                && c.CurrentHP_ > -c.stats.HP_,
            (c.statusMods |> List.exists ((=) Prone))
                && c.CurrentHP_ > -c.stats.HP_,
            betweenInclusive (0, (c.stats.HP_ + 1) / 3) c.CurrentHP_,
            c.CurrentHP_ <= 0,
            c.stats.name,
            c.number)
    potentialTargets |> Seq.tryHead

let fightOneRound (cqrs: CQRS.CQRS<_, Combat>) =
    // HIGH speed and DX goes first so we use the negative of those values
    for c in cqrs.State.combatants.Values |> Seq.sortBy (fun c -> -c.stats.Speed_, -c.stats.DX_, c.stats.name, c.number) |> Seq.map (fun c -> c.Id) do
        let d = RollSpec.create(3,6)
        let attackRoll = d.roll()
        let mutable msg = ""
        let recordMsg txt =
            if msg = "" then msg <- txt else msg <- $"{msg}; {txt}"
        let attempt label targetNumber =
            let roll = d.roll()
            if roll <= targetNumber then
                recordMsg $"{label} succeeded (needed {targetNumber}, rolled {roll}) "
                true
            else
                recordMsg $"{label} failed (needed {targetNumber}, rolled {roll})"
                false
        let checkGoesUnconscious (self: Combatant) incomingDamage =
            let penalty = (self.CurrentHP_ - incomingDamage) / self.stats.HP_
            attempt "Stay conscious" (self.stats.HT_ - penalty) |> not
        let mutable doneEarly = false
        let self = cqrs.State.combatants[c]
        if self.statusMods |> List.exists (function Dead | Unconscious -> true | _ -> false) |> not then
            if self.statusMods |> List.exists ((=) Stunned) then
                if attempt "Recover from stun" self.stats.HT_ then
                    Unstun(self.Id, msg)
                else
                    Info(self.Id, "does nothing", msg)
                |> cqrs.Execute
            elif self.CurrentHP_ <= 0 && checkGoesUnconscious self 0 then
                FallUnconscious(self.Id, msg)
                |> cqrs.Execute
            elif self.statusMods |> List.exists ((=) Prone) then
                StandUp(self.Id, msg)
                |> cqrs.Execute
            else
                for n in 1..(self.stats.ExtraAttack_ + 1) do
                    if (not doneEarly) then
                        match self |> tryFindTarget cqrs.State with
                        | Some victim ->
                            if attempt "Attack" (defaultArg self.stats.WeaponSkill 10) then
                                let dodgeTarget, retreat = if victim.retreatUsed then int victim.stats.Dodge_, false else (3 + int victim.stats.Dodge_), true
                                let defense = { defense = Dodge; targetRetreated = retreat }
                                if attempt (if retreat then "Dodge and retreat" else "Dodge") dodgeTarget then
                                    SuccessfulDefense({ attacker = self.Id; target = victim.Id }, defense, msg)
                                else
                                    let dmg = self.stats.Damage_.roll()
                                    let penetratingDmg = dmg - victim.stats.DR_
                                    let injury = match self.stats.DamageType with
                                                    | Some Cutting -> (float penetratingDmg * 1.5) |> int
                                                    | Some Impaling -> penetratingDmg * 2
                                                    | _ -> penetratingDmg
                                    let mutable newConditions = []
                                    let hp' = victim.CurrentHP_ - injury
                                    // -5 x max HP is auto-death
                                    if hp' <= victim.stats.HP_ * -5 then
                                        newConditions <- [Dead]
                                    // check for death if crossing a HP threshold, -1 x max HP or below
                                    elif hp' <= victim.stats.HP_ * -1 && ((hp' / victim.stats.HP_) <> (victim.CurrentHP_ / victim.stats.HP_)) && (attempt "Deathcheck" victim.stats.HT_ |> not) then
                                        newConditions <- [Dead]
                                    // check for unconsciousness on dropping to zero HP
                                    elif self.CurrentHP_ > 0 && hp' <= 0 && checkGoesUnconscious victim injury then
                                        newConditions <- [Unconscious]
                                    elif injury > (victim.stats.HP_ + 1) / 2 && attempt "Knockdown check" victim.stats.HT_ then
                                        newConditions <- [Stunned; Prone]
                                    Hit({ attacker = self.Id; target = victim.Id }, { defense = Parry; targetRetreated = false }, injury, newConditions, msg)
                            else
                                Miss({ attacker = self.Id; target = victim.Id }, msg)
                        | None ->
                            doneEarly <- true
                            Info(self.Id, "can't find a victim", msg)
                        |> cqrs.Execute
                        msg <- "" // if multiple attacks process we don't want to redisplay the same text


let fight (cqrs: CQRS.CQRS<_,Combat>) =
    let rec loop counter =
        let survivingTeams =
            let everyone = cqrs.State.combatants.Values |> List.ofSeq
            everyone |> List.choose (fun c -> if not (c.statusMods |> List.exists (fun m -> m = Dead || m = Unconscious)) then Some c.team else None)
                     |> List.distinct
        if survivingTeams.Length < 2 || counter > 100 then
            // it's possible to have a tie or a mutual kill
            {| victors = survivingTeams |}
        else
            if counter > 1 then
                cqrs.Execute (NewRound(counter))
            fightOneRound cqrs
            loop (counter + 1)
    loop 1

let toCombatants (db: Map<string, Creature>) team (quantity, name:string) =
    [for i in 1..quantity do
        Combatant.fresh(team, (if quantity > 1 then $"{name} {i}" else name), i, db[name])
        ]
let createCombat (db: Map<string, Creature>) team1 team2 =
    { combatants =
        (team1 |> List.collect (toCombatants db 1)) @ (team2 |> List.collect (toCombatants db 2))
        |> Seq.map(fun c -> c.Id, c)
        |> Map.ofSeq
        }
let specificFight db team1 team2 =
    let cqrs = CQRS.CQRS.Create((createCombat db team1 team2), update)
    let victors = fight cqrs
    cqrs.LogWithMessages(), victors
let calibrate db team1 (enemyType, minbound, maxbound) =
    let runForN n =
        let combat = createCombat db team1 [ n, enemyType ]
        let cqrs = CQRS.CQRS.Create(combat, update)
        cqrs, fight cqrs
    let mutable results = Map.empty
    let get n =
        if results.ContainsKey n then results[n]
        else
            let runs = [
                for run in 1..10 do
                    runForN n
                ]
            let sampleLog: CombatLog = (runs |> List.last |> fst).LogWithMessages()
            let victories = runs |> List.sumBy (function (_, v) when v.victors = [1] -> 1 | _ -> 0)
            results <- results |> Map.add n (victories, sampleLog)
            results[n]
    // crude and naive model: search from 1 to 100, but quit early when we fall to 0% victory
    let upToOneHundred =
        let rec loop n =
            if get n |> fst > 0 && n <= 100 then
                n::(loop (n+1))
            else []
        loop 1
    let inbounds n = betweenInclusive (minbound * 10. |> int) (maxbound * 10. |> int) (get n |> fst)
    match upToOneHundred |> List.filter inbounds with
    | [] ->
        for n in 1..100 do
            let victories, sampleLog = get n
            printfn $"against {n} {enemyType}: {victories} victories"
        None, None, None
    | inbounds ->
        let min = inbounds |> List.min
        let max = inbounds |> List.max
        let sampleFight: CombatLog = get min |> snd
        Some min, Some max, Some sampleFight
