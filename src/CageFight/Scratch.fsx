#load @"Core\Common.fs"
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
    }
    with
    member this.CurrentHP_ = this.stats.HP_ - this.damageTaken
    member this.Id : CombatantId = this.team, this.personalName
type Combat = {
    combatants: Map<CombatantId, Combatant>
    }
let toCombatants (db: Map<string, Creature>) team (quantity, name:string) =
    [for i in 1..quantity do
        {   personalName =
                if quantity > 1 then sprintf "%s %d" name i else name
            number = i
            team = team
            stats = db[name]
            damageTaken = 0
            statusMods = [] }
        ]
let createCombat (db: Map<string, Creature>) team1 team2 =
    { combatants =
        (team1 |> List.collect (toCombatants db 0)) @ (team2 |> List.collect (toCombatants db 1))
        |> Seq.map(fun c -> c.Id, c)
        |> Map.ofSeq
        }
let db = Defaults.database()
(3, "Orc") |> toCombatants db 0
let combat = createCombat db [ 3, "Orc"; 1, "Slugbeast"; 1, "Skeleton"] [ 14, "Orc" ]
combat.combatants.Values |> Seq.filter (fun c -> c.team = 0)
combat.combatants.Values |> Seq.sortBy(fun c -> c.Id) |> Seq.map (fun c -> $"{c.Id}") |> Seq.iter (printfn "%s")
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
for c in combat.combatants.Values |> Seq.sortBy (fun c -> c.team, c.stats.name, c.number) do
    printfn "%A attacks %A" c.Id (tryFindTarget combat c).Value.Id
