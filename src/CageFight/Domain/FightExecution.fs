module Domain.FightExecution
open Domain
open Domain.Random

type Status = OK | Stunned | Prone | Unconscious | Dead
type CombatantId = int * string
type Combatant = {
    personalName: string
    number: int
    team: int
    stats: Creature
    injuryTaken: int
    statusMods: Status list
    retreatUsed: bool
    blockUsed: bool
    parriesUsed: int
    }
    with
    member this.CurrentHP_ = this.stats.HP_ - this.injuryTaken
    member this.Id : CombatantId = this.team, this.personalName
    static member fresh (team, name, number, stats: Creature) =
        {   team = team
            personalName = name
            number = number
            stats = stats
            injuryTaken = 0
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
                    injuryTaken = c.injuryTaken + amount
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

let prioritizeTargets (combat: Combat) (attacker: Combatant) =
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
            ((c.statusMods |> List.contains Stunned)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            ((c.statusMods |> List.contains Prone)
                && c.CurrentHP_ > -c.stats.HP_) |> not,
            betweenInclusive (0, (c.stats.HP_ + 1) / 3) c.CurrentHP_ |> not,
            c.CurrentHP_ <= 0 |> not,
            c.stats.name,
            c.number)
    potentialTargets

let tryFindTarget (combat: Combat) (attacker: Combatant) =
    prioritizeTargets combat attacker |> Seq.tryHead

let chooseDefense (victim: Combatant) =
    let (|Parry|_|) = function
        | Some parry ->
            printfn $"{victim.personalName} has a parry of {parry}"
            let parry = (parry - (if victim.stats.WeaponMaster then 2 else 4) * (victim.parriesUsed / (1 + victim.stats.ExtraParry_)))
            printfn $"{victim.personalName} has a modified parry of {parry}"
            Some(if victim.retreatUsed then parry, false else 1 + parry, true)
        | None -> None
    let (|Block|_|) = function
        | Some block when victim.blockUsed = false ->
            Some(if victim.retreatUsed then block, false else 1 + block, true)
        | _ -> None
    let dodge, retreat =
        let dodge = if (float victim.CurrentHP_) >= (float victim.stats.HP_ / 3.)
                    then victim.stats.Dodge_
                    else victim.stats.Dodge_ / 2
        if victim.retreatUsed then dodge, false else 3 + dodge, true
    let target, defense =
        match victim.stats.Parry, victim.stats.Block with
        | Parry (parry, retreat), Block (block, _) when parry >= block && parry >= dodge ->
            // I guess we'll use parry in this case because we have to pick something
            parry, { defense = Parry; targetRetreated = retreat }
        | _, Block (block, retreat) when block >= dodge ->
            block, { defense = Block; targetRetreated = retreat }
        | Parry (parry, retreat), _ when parry >= dodge ->
            parry, { defense = Parry; targetRetreated = retreat }
        | _ ->
            dodge, { defense = Dodge; targetRetreated = retreat }
    let target =
        target
        + (if victim.statusMods |> List.contains Stunned then -4 else 0)
        + (if victim.statusMods |> List.contains Prone then -3 else 0)
    printfn "Choosing defense for %s: %A" victim.personalName defense
    target, defense

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
                recordMsg $"{label} succeeded (needed {targetNumber}, rolled {roll})"
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
                                let defenseTarget, defense = chooseDefense victim
                                let defenseLabel =
                                    (match defense.defense with Parry -> "Parry" | Block -> "Block" | Dodge -> "Dodge")
                                    + (if defense.targetRetreated then " and retreat" else "")
                                if attempt defenseLabel defenseTarget then
                                    SuccessfulDefense({ attacker = self.Id; target = victim.Id }, defense, msg)
                                else
                                    let dmg = self.stats.Damage_.roll()
                                    let penetratingDmg = dmg - victim.stats.DR_ |> max 0
                                    let injury = match self.stats.DamageType with
                                                    | Some Cutting -> (float penetratingDmg * 1.5) |> int
                                                    | Some Impaling -> penetratingDmg * 2
                                                    | _ -> penetratingDmg
                                    recordMsg $"Damage {self.stats.Damage_} ({dmg} {self.stats.DamageType}) - DR {victim.stats.DR_} = {injury} injury"
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
                                    elif injury > (victim.stats.HP_ + 1) / 2 && (attempt "Knockdown check" victim.stats.HT_ |> not) then
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
