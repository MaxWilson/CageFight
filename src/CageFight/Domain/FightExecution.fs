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
    shockPenalty: int
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
            shockPenalty = 0
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
        let newTurn (id: CombatantId) =
            updateCombatant id (fun c ->
                { c with
                    retreatUsed = false
                    blockUsed = false
                    parriesUsed = 0
                    shockPenalty = 0
                    })
        let takeDamage (id: CombatantId) amount conditions =
            updateCombatant id (fun c ->
                { c with
                    injuryTaken = c.injuryTaken + amount
                    shockPenalty =
                        if c.stats.SupernaturalDurability then 0
                        else (c.shockPenalty - amount) |> max -4
                    statusMods = List.distinct (c.statusMods @ conditions)
                    })
        match msg with
        | Hit (ids, defense, injury, statusImpact, rollDetails) ->
            model |> newTurn ids.attacker
                  |> consumeDefense ids.target defense
                  |> takeDamage ids.target injury statusImpact
        | SuccessfulDefense(ids, defense, rollDetails) ->
            model |> newTurn ids.attacker
                  |> consumeDefense ids.target defense
        | Miss (ids, rollDetails) ->
            model |> newTurn ids.attacker
        | FallUnconscious(id, rollDetails) ->
            model |> newTurn id |> takeDamage id 0 [Unconscious]
        | Unstun(id, rollDetails) ->
            model |> newTurn id
                  |> updateCombatant id (fun c ->
                        { c with statusMods = c.statusMods |> List.filter ((<>) Stunned) })
        | StandUp(id, rollDetails) ->
            model |> newTurn id
                  |> updateCombatant id (fun c ->
                        { c with statusMods = c.statusMods |> List.filter ((<>) Prone) })
        | Info (id, _, _) -> model |> newTurn id
        | NewRound _ -> model
type CombatLog = (Event option * Combat) list
type DefeatCriteria =
    | TPK
    | OneCasualty
    | HalfCasualties
type FightResult =
    | CalibratedResult of lower:int option * upper:int option * sample:CombatLog
    | SpecificResult of CombatLog * {| victors: int list |}
type Outcome = CritSuccess of int | Success of int | CritFail of int | Fail of int

let successTest target x =
    if x >= target + 10 then CritFail (x - target)
    elif x = 17 then if target >= 16 then Fail(x - 16) else CritFail (x - 16)
    elif x = 18 then CritFail (x - target)
    elif target < 15 && x = 19 then Fail (x - target)
    elif target < 14 && x = 20 then Fail (x - target)
    elif x > target then Fail (x - target)
    elif target >= 16 && x <= 6 then CritSuccess ((min target 6) - x)
    elif target >= 15 && x <= 5 then CritSuccess ((min target 5) - x)
    elif x <= 4 then CritSuccess ((min target 4) - x)
    else Success (target - x)

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
            let parry = (parry - (if victim.stats.WeaponMaster then 2 else 4) * (victim.parriesUsed / (1 + victim.stats.ExtraParry_)))
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
    target, defense

let fightOneRound (cqrs: CQRS.CQRS<_, Combat>) =
    // HIGH speed and DX goes first so we use the negative of those values
    for c in cqrs.State.combatants.Values |> Seq.sortBy (fun c -> -c.stats.Speed_, -c.stats.DX_, c.stats.name, c.number) |> Seq.map (fun c -> c.Id) do
        let d = RollSpec.create(3,6)
        let attackRoll = d.roll()
        let mutable msg = ""
        let recordMsg txt =
            if msg = "" then msg <- txt else msg <- $"{msg}; {txt}"
        let detailedAttempt label targetNumber =
            let targetNumber = min 16 targetNumber
            let roll = d.roll()
            match successTest targetNumber roll with
            | CritSuccess _ as success ->
                recordMsg $"{label} critically succeeded (needed {targetNumber}, rolled {roll})"
                success
            | Success _ as success ->
                recordMsg $"{label} succeeded (needed {targetNumber}, rolled {roll})"
                success
            | Fail _ as fail ->
                recordMsg $"{label} failed (needed {targetNumber}, rolled {roll})"
                fail
            | CritFail _ as fail ->
                recordMsg $"{label} failed (needed {targetNumber}, rolled {roll})"
                fail
        let attempt label targetNumber =
            match detailedAttempt label targetNumber with
            | (CritSuccess _ | Success _) as success -> true
            | (CritFail _ | Fail _) -> false
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
            elif self.CurrentHP_ <= 0 && (not self.stats.SupernaturalDurability) && checkGoesUnconscious self 0 then
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
                            match detailedAttempt "Attack" (self.stats.WeaponSkill_ + self.shockPenalty) with
                            | (Success _ | CritSuccess _) as success ->
                                let defenseTarget, defense = chooseDefense victim
                                let defenseLabel =
                                    (match defense.defense with Parry -> "Parry" | Block -> "Block" | Dodge -> "Dodge")
                                    + (if defense.targetRetreated then " and retreat" else "")
                                let critSuccess = match success with CritSuccess _ -> true | _ -> false
                                if (not critSuccess) && attempt defenseLabel defenseTarget then
                                    SuccessfulDefense({ attacker = self.Id; target = victim.Id }, defense, msg)
                                else
                                    let dmg = self.stats.Damage_.roll() |> max (if self.stats.DamageType = Some Crushing then 0 else 1)
                                    let penetratingDmg = dmg - victim.stats.DR_ |> max 0
                                    let toInjury (penetratingDmg, damageType) =
                                        match self.stats.InjuryTolerance, damageType with
                                        | Some Diffuse, Some (Impaling | Piercing) -> max 1 penetratingDmg
                                        | Some Diffuse, _ -> max 2 penetratingDmg
                                        | Some Homogeneous, Some Impaling -> penetratingDmg / 2
                                        | Some Homogeneous, Some Piercing -> penetratingDmg / 5
                                        | Some Unliving, Some Impaling -> penetratingDmg
                                        | Some Unliving, Some Piercing -> penetratingDmg / 3
                                        | _, Some Cutting -> (float penetratingDmg * 1.5) |> int
                                        | _, Some Impaling -> penetratingDmg * 2
                                        | _ -> penetratingDmg
                                    let injury = toInjury (penetratingDmg, self.stats.DamageType)
                                    // add followup damage, and log the total damage and injury
                                    let injury =
                                        match self.stats.FollowupDamage with
                                        | Some r when penetratingDmg > 0 ->
                                            let followup, followupType = (r.roll(), self.stats.FollowupDamageType)
                                            let injury = injury + toInjury (followup, followupType)
                                            recordMsg $"Damage {self.stats.Damage_} + {r} ({dmg} {defaultArg self.stats.DamageType Other}, {followup} {followupType}) - DR {victim.stats.DR_} = {injury} injury"
                                            injury
                                        | _ ->
                                            recordMsg $"Damage {self.stats.Damage_} ({dmg} {defaultArg self.stats.DamageType Other}) - DR {victim.stats.DR_} = {injury} injury"
                                            injury
                                    let mutable newConditions = []
                                    let hp' = victim.CurrentHP_ - injury
                                    // -5 x max HP is auto-death
                                    if hp' <= victim.stats.HP_ * (if victim.stats.UnnaturallyFragile then -1 else -5) then
                                        newConditions <- [Dead]
                                    // check for death if crossing a HP threshold, -1 x max HP or below
                                    elif hp' <= victim.stats.HP_ * -1 && ((hp' / victim.stats.HP_) <> (victim.CurrentHP_ / victim.stats.HP_)) && (attempt "Deathcheck" victim.stats.HT_ |> not) then
                                        newConditions <- [Dead]
                                    // check for unconsciousness on dropping to zero HP
                                    elif victim.CurrentHP_ > 0 && hp' <= 0 && (not victim.stats.SupernaturalDurability) && checkGoesUnconscious victim injury then
                                        newConditions <- [Unconscious]
                                    elif injury > (victim.stats.HP_ + 1) / 2 && (not victim.stats.SupernaturalDurability) && (attempt "Knockdown check" victim.stats.HT_ |> not) then
                                        newConditions <- [Stunned; Prone]
                                    Hit({ attacker = self.Id; target = victim.Id }, { defense = Parry; targetRetreated = false }, injury, newConditions, msg)
                            | (Fail _ | CritFail _) ->
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
let calibrate db team1 (enemyType, minbound, maxbound, defeatCriteria) =
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
            let victoryMetric : CQRS.CQRS<_, Combat> * {| victors: int list |} -> int =
                match defeatCriteria with
                | TPK -> function (_, v) when v.victors = [1] -> 1 | otherwise -> 0
                | OneCasualty ->
                    fun (cqrs, v) ->
                        // in this case, TeamA is very casualty-averse. Defeat is taking even one casualty (dead or unconscious).
                        if cqrs.State.combatants.Values |> Seq.exists (fun c ->
                            c.team = 1 && c.statusMods |> List.exists (
                                function Dead | Unconscious -> true | _ -> false)) then
                            0
                        else 1
                | HalfCasualties ->
                    fun (cqrs, v) ->
                        // in this case, TeamA is somewhat casualty-averse. Defeat is a pyrrhic victory where at least half the team dies.
                        let friendlies = cqrs.State.combatants.Values |> Seq.filter (fun c -> c.team = 1)
                        let deadFriendlies = friendlies |> Seq.filter (fun c -> c.statusMods |> List.exists (function Dead | Unconscious -> true | _ -> false))
                        if deadFriendlies |> Seq.length >= ((friendlies |> Seq.length) / 2) then
                            0
                        else 1

            let victories = runs |> List.sumBy victoryMetric
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
        None, None, None
    | inbounds ->
        let min = inbounds |> List.min
        let max = inbounds |> List.max
        let sampleFight: CombatLog = get max |> snd
        Some min, Some max, Some sampleFight
