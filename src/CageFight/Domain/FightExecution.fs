module Domain.FightExecution
open Domain
open Domain.Random

type Status = OK | Stunned | Prone | Unconscious | Dead | Berserk
type CombatantId = int * string
type Combatant = {
    personalName: string
    number: int
    team: int
    stats: Creature
    injuryTaken: int
    shockPenalty: int
    statusMods: Status list
    retreatUsed: CombatantId option
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
            retreatUsed = None
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
type DefenseDetails = { defense: DefenseType; retreatFrom: CombatantId option }
    with member this.targetRetreated = this.retreatFrom.IsSome

[<AutoOpen>]
module CombatEvents =
    type Event =
        | Hit of Ids * DefenseDetails option * injury:int * Status list * string
        | SuccessfulDefense of Ids * DefenseDetails * string
        | Miss of Ids * string
        | FallUnconscious of CombatantId * string
        | Unstun of CombatantId * string
        | StandUp of CombatantId * string
        | Info of CombatantId * msg: string * rollInfo: string
        | NewRound of int
    let update msg model =
        let updateCombatant id (f: Combatant -> Combatant) model =
            {   combatants = model.combatants |> Map.change id (function | Some c -> Some (f c) | None -> None)
                }
        let consumeDefense (id: CombatantId) (defense: DefenseDetails option) =
            updateCombatant id (fun c ->
                match defense with
                | Some defense ->
                    { c with
                        retreatUsed = c.retreatUsed |> Option.orElse defense.retreatFrom
                        blockUsed = c.blockUsed || defense.defense = Block
                        parriesUsed = c.parriesUsed + (if defense.defense = Parry then 1 else 0)
                        }
                | None -> c)
        let newTurn (id: CombatantId) =
            updateCombatant id (fun c ->
                { c with
                    retreatUsed = None
                    blockUsed = false
                    parriesUsed = 0
                    shockPenalty = 0
                    })
        let takeDamage (id: CombatantId) amount conditions =
            updateCombatant id (fun c ->
                let goingBerserk = conditions |> List.contains Berserk
                // if going berserk, make sure to remove Stunned from conditions
                let mods' = (c.statusMods @ conditions) |> List.distinct
                { c with
                    injuryTaken = c.injuryTaken + amount
                    shockPenalty =
                        if c.stats.SupernaturalDurability || c.stats.HighPainThreshold || (mods' |> List.contains Berserk) then 0
                        else (c.shockPenalty - amount) |> max -4
                    statusMods = if goingBerserk then mods' |> List.filter ((<>) Stunned) else mods'
                    })
        match msg with
        | Hit (ids, defense, injury, statusImpact, rollDetails) ->
            model |> newTurn ids.attacker
                  |> consumeDefense ids.target defense
                  |> takeDamage ids.target injury statusImpact
        | SuccessfulDefense(ids, defense, rollDetails) ->
            model |> newTurn ids.attacker
                  |> consumeDefense ids.target (Some defense)
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
            c.CurrentHP_ <= 0 && not c.stats.SupernaturalDurability,
            c.number)
    potentialTargets

let tryFindTarget (combat: Combat) (attacker: Combatant) =
    prioritizeTargets combat attacker |> Seq.tryHead

let chooseDefense (attacker: CombatantId) (victim: Combatant) =
    let canRetreat =
        (not <| List.includes [Dead; Unconscious; Stunned] victim.statusMods)
        && (
        match victim.retreatUsed with
        | Some id when id = attacker -> true
        | None -> true
        | _ -> false)
    let (|Parry|_|) = function
        | Some parry ->
            let parry = (parry - (match victim.stats.WeaponMaster, victim.stats.FencingParry with | true, true -> 1 | true, false | false, true -> 2 | otherwise -> 4) * (victim.parriesUsed / (1 + victim.stats.ExtraParry_)))
            Some(if canRetreat then (if victim.stats.FencingParry then 3 else 1) + parry, Some attacker else parry, None)
        | None -> None
    let (|Block|_|) = function
        | Some block when victim.blockUsed = false ->
            Some(if canRetreat then 1 + block, Some attacker else block, None)
        | _ -> None
    let dodge, retreat =
        let dodge = if (float victim.CurrentHP_) >= (float victim.stats.HP_ / 3.)
                    then victim.stats.Dodge_
                    else victim.stats.Dodge_ / 2
        if canRetreat then 3 + dodge, Some attacker else dodge, None
    let target, defense =
        match victim.stats.Parry, victim.stats.Block with
        | Parry (parry, retreat), Block (block, _) when parry >= block && parry >= dodge ->
            // I guess we'll use parry in this case because we have to pick something
            parry, { defense = Parry; retreatFrom = retreat }
        | _, Block (block, retreat) when block >= dodge ->
            block, { defense = Block; retreatFrom = retreat }
        | Parry (parry, retreat), _ when parry >= dodge ->
            parry, { defense = Parry; retreatFrom = retreat }
        | _ ->
            dodge, { defense = Dodge; retreatFrom = retreat }
    let target =
        target
        + (if victim.statusMods |> List.contains Stunned then -4 else 0)
        + (if victim.statusMods |> List.contains Prone then -3 else 0)
    target, defense

let failedDeathcheck (attempt: int -> bool) (fullHP: int) priorHP newHP =
    if newHP <= fullHP * -1 then
        let oldBracket = priorHP / fullHP |> min 0
        let newBracket = newHP / fullHP
        let checksNeeded = oldBracket - newBracket
        let rec loop checksFinished =
            let threshold = ((-oldBracket + 1) + checksFinished) * -fullHP
            if checksFinished = checksNeeded then false // Zero or more
            elif attempt threshold = false then
                true // failed!
            else loop (checksFinished+1)
        loop 0
    else false

let fightOneRound (cqrs: CQRS.CQRS<_, Combat>) =
    // HIGH speed and DX goes first so we use the negative of those values
    for c in cqrs.State.combatants.Values |> Seq.sortBy (fun c -> -c.stats.Speed_, -c.stats.DX_, c.stats.name, c.number) |> Seq.map (fun c -> c.Id) do
        let roll3d6 = RollSpec.create(3,6)
        let mutable msg = ""
        let recordMsg txt =
            if msg = "" then msg <- txt else msg <- $"{msg}; {txt}"
        let detailedAttempt label targetNumber =
            let roll = roll3d6.roll()
            match successTest targetNumber roll with
            | CritSuccess _ as success ->
                recordMsg $"{label} critically succeeded (target {targetNumber}, rolled {roll})"
                success
            | Success _ as success ->
                recordMsg $"{label} succeeded (target {targetNumber}, rolled {roll})"
                success
            | Fail _ as fail ->
                recordMsg $"{label} failed (target {targetNumber}, rolled {roll})"
                fail
            | CritFail _ as fail ->
                recordMsg $"{label} failed (target {targetNumber}, rolled {roll})"
                fail
        let attempt label targetNumber =
            match detailedAttempt label targetNumber with
            | (CritSuccess _ | Success _) as success -> true
            | (CritFail _ | Fail _) -> false
        let checkGoesUnconscious (self: Combatant, isBerserk) incomingDamage =
            let penalty = (self.CurrentHP_ - incomingDamage) / self.stats.HP_
            attempt "Stay conscious" (self.stats.HT_ + penalty + (if isBerserk then +4 else 0)) |> not
        let mutable doneEarly = false
        let attacker = cqrs.State.combatants[c]
        if attacker.statusMods |> List.exists (function Dead | Unconscious -> true | _ -> false) |> not then
            if attacker.statusMods |> List.exists ((=) Stunned) then
                if attempt "Recover from stun" attacker.stats.HT_ then
                    Unstun(attacker.Id, msg)
                else
                    Info(attacker.Id, "does nothing", msg)
                |> cqrs.Execute
            elif attacker.CurrentHP_ <= 0 && (not attacker.stats.SupernaturalDurability) && checkGoesUnconscious (attacker, attacker.statusMods |> List.contains Berserk) 0 then
                FallUnconscious(attacker.Id, msg)
                |> cqrs.Execute
            elif attacker.statusMods |> List.exists ((=) Prone) then
                StandUp(attacker.Id, msg)
                |> cqrs.Execute
            else
                for m in 1..(1 + attacker.stats.AlteredTimeRate_) do
                    let totalAttacks, rapidStrikes = (if attacker.stats.UseRapidStrike then 2 + attacker.stats.ExtraAttack_, 2 else 1 + attacker.stats.ExtraAttack_, 0)
                    for n in 1..totalAttacks do
                        if (not doneEarly) then
                            match attacker |> tryFindTarget cqrs.State with
                            | Some victim ->
                                let skill, defensePenalty =
                                    let rapidStrikePenalty =
                                        if n <= rapidStrikes then
                                            let penalty = if attacker.stats.WeaponMaster then -3 else -6
                                            recordMsg $"Using Rapid Strike %+d{penalty}"
                                            penalty
                                        else 0
                                    if attacker.shockPenalty <> 0 then
                                        recordMsg $"Shock penalty %+d{attacker.shockPenalty}"
                                    match (attacker.stats.WeaponSkill_ + attacker.shockPenalty + rapidStrikePenalty) with
                                    | n when n >= 18 ->
                                        let deceptive = (n - 16)/2
                                        recordMsg $"Using Deceptive Attack {-2 * deceptive}"
                                        n - deceptive * 2, deceptive
                                    | n -> n, 0
                                match detailedAttempt "Attack" skill with
                                | (Success _ | CritSuccess _) as success ->
                                    let defenseTarget, defense = chooseDefense attacker.Id victim
                                    let defenseLabel =
                                        (match defense.defense with Parry -> "Parry" | Block -> "Block" | Dodge -> "Dodge")
                                        + (if defense.targetRetreated then " and retreat" else "")
                                    let critSuccess = match success with CritSuccess _ -> true | _ -> false
                                    if (not critSuccess) && attempt defenseLabel (defenseTarget - defensePenalty) then
                                        SuccessfulDefense({ attacker = attacker.Id; target = victim.Id }, defense, msg)
                                    else
                                        let defense =
                                            if critSuccess then None
                                            else Some defense
                                        let damageCap damageType = max (if damageType = Some Crushing then 0 else 1)
                                        let dmg = attacker.stats.Damage_.roll() |> damageCap attacker.stats.DamageType
                                        let penetratingDmg = dmg - victim.stats.DR_ |> max 0
                                        let toInjury (penetratingDmg, damageType) =
                                            match victim.stats.InjuryTolerance, damageType with
                                            | Some Diffuse, Some (Impaling | Piercing) -> max 1 penetratingDmg
                                            | Some Diffuse, _ -> max 2 penetratingDmg
                                            | Some Homogeneous, Some Impaling -> penetratingDmg / 2
                                            | Some Homogeneous, Some Piercing -> penetratingDmg / 5
                                            | Some Unliving, Some Impaling -> penetratingDmg
                                            | Some Unliving, Some Piercing -> penetratingDmg / 3
                                            | _, Some Cutting -> (float penetratingDmg * 1.5) |> int
                                            | _, Some Impaling -> penetratingDmg * 2
                                            | _ -> penetratingDmg
                                        let injury = toInjury (penetratingDmg, attacker.stats.DamageType)
                                        // add followup damage, and log the total damage and injury
                                        let injury =
                                            match attacker.stats.FollowupDamage with
                                            | Some r when penetratingDmg > 0 ->
                                                let followup, followupType = (r.roll() |> damageCap attacker.stats.FollowupDamageType, attacker.stats.FollowupDamageType)
                                                let injury = injury + toInjury (followup, followupType)
                                                recordMsg $"Damage {attacker.stats.Damage_} + {r} ({dmg} {defaultArg attacker.stats.DamageType Other}, {followup} {followupType}) - DR {victim.stats.DR_} = {injury} injury"
                                                injury
                                            | _ ->
                                                recordMsg $"Damage {attacker.stats.Damage_} ({dmg} {defaultArg attacker.stats.DamageType Other}) - DR {victim.stats.DR_} = {injury} injury"
                                                injury
                                        let mutable newConditions = []
                                        let mutable berserk = victim.statusMods |> List.contains Berserk
                                        match victim.stats.Berserk with
                                        | Some berserkLevel when (float injury > float victim.stats.HP_ / 4. && (victim.statusMods |> List.contains Berserk |> not)) ->
                                            let target =
                                                match berserkLevel with Mild -> 15 | Moderate -> 12 | Serious -> 9 | Severe -> 6 | Always -> 0
                                            // we deliberately don't use attempt here because we don't want to clutter the log with self-control rolls
                                            if (roll3d6.roll() <= target = false) then
                                                recordMsg $"{victim.personalName} goes berserk"
                                                newConditions <- newConditions@[Berserk]
                                                berserk <- true
                                        | _ -> ()
                                        let hp' = victim.CurrentHP_ - injury
                                        // -5 x max HP is auto-death
                                        let autodeathThreshold = victim.stats.HP_ * (if victim.stats.UnnaturallyFragile then -1 else -5)
                                        if hp' <= autodeathThreshold then
                                            recordMsg $"Auto-death occurs at {autodeathThreshold} HP"
                                            newConditions <- [Dead]
                                        // check for death if crossing a HP threshold, -1 x max HP or below
                                        elif failedDeathcheck (fun threshold -> attempt $"Deathcheck at {threshold} HP" (victim.stats.HT_ + if berserk then +4 else 0))
                                                victim.stats.HP_ victim.CurrentHP_ hp' then
                                            newConditions <- [Dead]
                                        // check for unconsciousness on dropping to zero HP
                                        elif victim.CurrentHP_ > 0 && hp' <= 0 && (not victim.stats.SupernaturalDurability) && checkGoesUnconscious (victim, berserk) injury then
                                            newConditions <- [Unconscious]
                                        elif injury > (victim.stats.HP_ + 1) / 2 && not (victim.stats.SupernaturalDurability || berserk)
                                                && (attempt "Knockdown check" (victim.stats.HT_ +
                                                    if victim.stats.HighPainThreshold then +3 else 0) |> not) then
                                            newConditions <- [Stunned; Prone]
                                        Hit({ attacker = attacker.Id; target = victim.Id }, defense, injury, newConditions, msg)
                                | (Fail _ | CritFail _) ->
                                    Miss({ attacker = attacker.Id; target = victim.Id }, msg)
                            | None ->
                                doneEarly <- true
                                Info(attacker.Id, "can't find a victim", msg)
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

let toCombatants (db: Map<string, Creature>) team =
    // we want numbers to ascend smoothly on a side, so that we can use numbers to prioritize targets in the same order they were in fightsetup
    let mutable counter = 0
    fun (quantity, name:string) ->
        [   for i in 1..quantity do
                Combatant.fresh(team, (if quantity > 1 then $"{name} {i}" else name), counter + i, db[name])
            counter <- counter + quantity
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
