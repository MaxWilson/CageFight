#load @"Core\Common.fs"
#load @"Core\CQRS.fs"
#load @"Core\Packrat.fs"
#load @"Domain\Random.fs"
#load @"Domain\Domain.fs"
open Domain
open Domain.Random
let swingDamage st bonusOrPenalty =
    if st < 9 then RollSpec.create(1,6, bonusOrPenalty + (st-12)/2)
    elif st < 28 then
        let nDice = 1 + ((st-9) / 4)
        let bonus = (st-9) % 4 - 1
        RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
    else
        notImpl "Swing damage for ST 28+"
for st in [1..27] do
    printfn "%d: %O" st (swingDamage st 0)
let thrustDamage st bonusOrPenalty =
    if st < 13 then RollSpec.create(1,6, bonusOrPenalty + (st-14)/2)
    elif st <= 40 then
        let nDice = 1 + (st-11) / 8
        let bonus = (st-11) / 2 % 4 - 1
        RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
    else
        notImpl "Thrust damage for ST 28+"
for st in [1..40] do
    printfn "%d: %O" st (thrustDamage st 0)

open System
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
let toCombatants (db: Map<string, Creature>) team (quantity, name:string) =
    [for i in 1..quantity do
        Combatant.fresh(team, (if quantity > 1 then $"{name} {i}" else name), i, db[name])
        ]
let createCombat (db: Map<string, Creature>) team1 team2 =
    { combatants =
        (team1 |> List.collect (toCombatants db 0)) @ (team2 |> List.collect (toCombatants db 1))
        |> Seq.map(fun c -> c.Id, c)
        |> Map.ofSeq
        }
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
combat.combatants[(0, "Orc 3")] |> tryFindTarget combat
type Ids =
    { attacker: CombatantId; target: CombatantId }
    with
    member this.Attacker_ = snd this.attacker
    member this.AttackerTeam_ = fst this.attacker
    member this.Target_ = snd this.target
    member this.TargetTeam_ = fst this.target
type DefenseType = Parry | Block | Dodge
type DefenseDetails = { defense: DefenseType; targetRetreated: bool }
type Notification =
    | Hit of Ids * DefenseDetails * injury:int * Status list * string
    | SuccessfulDefense of Ids * DefenseDetails * string
    | Miss of Ids * string
    | FallUnconscious of CombatantId * string
    | Unstun of CombatantId * string
    | StandUp of CombatantId * string
    | Info of CombatantId * string
let notify msg =
    match msg with
    | Hit (ids, _, injury, statusImpact, rollDetails) ->
        match statusImpact with
        | v when v |> List.contains Dead ->
            printfn $"{ids.Attacker_} kills {ids.Target_} with a hit for {injury} HP {rollDetails}"
        | v when v |> List.contains Stunned ->
            printfn $"{ids.Attacker_} stuns {ids.Target_} with a hit for {injury} HP {rollDetails}"
        | _ ->
            printfn $"{ids.Attacker_} hits {ids.Target_} for {injury} HP {rollDetails}"
    | SuccessfulDefense(ids, { defense = Parry }, rollDetails) ->
        printfn $"{ids.Target_} parries {ids.Attacker_}'s attack {rollDetails}"
    | SuccessfulDefense(ids, { defense = Block }, rollDetails) ->
        printfn $"{ids.Target_} blocks {ids.Attacker_}'s attack {rollDetails}"
    | SuccessfulDefense(ids, { defense = Dodge }, rollDetails) ->
        printfn $"{ids.Target_} dodges {ids.Attacker_}'s attack {rollDetails}"
    | Miss (ids, rollDetails) ->
        printfn $"{ids.Attacker_} misses {ids.Target_} {rollDetails}"
    | FallUnconscious(id, rollDetails) ->
        printfn $"{id} falls unconscious {rollDetails}"
    | Unstun(id, rollDetails) ->
        printfn $"{id} recovers from stun {rollDetails}"
    | StandUp(id, rollDetails) ->
        printfn $"{id} stands up {rollDetails}"
    | Info (id, msg) ->
        printfn $"{id} {msg}"
for c in combat.combatants.Values |> Seq.sortBy (fun c -> c.team, c.stats.name, c.number) do
    printfn "%A attacks %A" c.Id (tryFindTarget combat c).Value.Id

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
    | Info _ -> model
let fightOneRound (cqrs: CQRS.CQRS<_, Combat>) =
    for c in cqrs.State.combatants.Values |> Seq.sortBy (fun c -> c.stats.Speed_, c.stats.DX_, c.personalName) |> Seq.map (fun c -> c.Id) do
        let d = RollSpec.create(3,6)
        let attackRoll = d.roll()
        let mutable msg = ""
        let recordMsg txt =
            if msg = "" then msg <- txt else msg <- $"{msg}; {txt}"
        let attempt label targetNumber =
            let roll = d.roll()
            if roll <= targetNumber then
                recordMsg $"{label} ({roll}/{targetNumber}) succeeded"
                true
            else
                recordMsg $"{label} ({roll}/{targetNumber}) failed"
                false
        let checkGoesUnconscious (self: Combatant) incomingDamage =
            let penalty = (self.CurrentHP_ - incomingDamage) / self.stats.HP_
            attempt "Stay conscious" (self.stats.HT_ - penalty)

        let self = cqrs.State.combatants[c]
        if self.statusMods |> List.exists (function Dead | Unconscious -> true | _ -> false) |> not then
            let cmd =
                if self.CurrentHP_ <= 0 && checkGoesUnconscious self 0 then
                    FallUnconscious(self.Id, msg)
                elif self.statusMods |> List.exists ((=) Stunned) then
                    if attempt "Recover from stun" self.stats.HT_ then
                        Unstun(self.Id, msg)
                    else
                        Info(self.Id, msg)
                elif self.statusMods |> List.exists ((=) Prone) then
                    StandUp(self.Id, msg)
                else
                    match self |> tryFindTarget cqrs.State with
                    | Some victim ->
                        if attempt "Attack" (defaultArg self.stats.WeaponSkill 10) then
                            let defenseRoll = d.roll()
                            let dodgeTarget, retreat = if victim.retreatUsed then int victim.stats.Speed_, false else (3 + int victim.stats.Speed_), true
                            let defense = { defense = Dodge; targetRetreated = retreat }
                            if attempt (if retreat then "Dodge and retreat" else "Dodge") dodgeTarget then
                                SuccessfulDefense({ attacker = self.Id; target = victim.Id }, defense, msg)
                            else
                                let dmg = self.stats.Damage_.roll()
                                let penetratingDmg = dmg // todo: account for DR
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
                        recordMsg "can't find a victim"
                        Info(self.Id, msg)
            cqrs.Execute cmd
let fight (cqrs: CQRS.CQRS<_,Combat>) =
    let rec loop counter =
        let survivingTeams =
            let everyone = cqrs.State.combatants.Values |> List.ofSeq
            everyone |> List.choose (fun c -> if not (c.statusMods |> List.exists (fun m -> m = Dead || m = Unconscious)) then Some c.team else None)
                     |> List.distinct
        if survivingTeams.Length < 2 || counter = 100 then
            // it's possible to have a tie or a mutual kill
            {| victors = survivingTeams |}
        else
            fightOneRound cqrs
            loop (counter + 1)
    loop 0

let db = Defaults.database()
(3, "Orc") |> toCombatants db 0
let combat = createCombat db [ 3, "Orc"; 1, "Slugbeast"; 1, "Skeleton"] [ 14, "Orc" ]
combat.combatants.Values |> Seq.filter (fun c -> c.team = 0)
combat.combatants.Values |> Seq.sortBy(fun c -> c.Id) |> Seq.map (fun c -> $"{c.Id}") |> Seq.iter (printfn "%s")
let cqrs = CQRS.CQRS.Create(combat, (fun msg model -> notify msg; update msg model))
fight cqrs
