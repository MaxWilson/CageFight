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
type Combatant = {
    personalName: string
    team: int
    stats: Creature
    damageTaken: int
    statusMods: Status list
    }
type Combat = {
    combatants: Map<string, Combatant>
    }
let renderTeam = function
    | 0 -> "Blue"
    | 1 -> "Red"
    | _ -> failwith "Invalid team"
let toCombatants (db: Map<string, Creature>) team opposition (quantity, name:string) =
    let includeTeamInName = (opposition |> List.exists (fun (_, name') -> name = name'))
    [for i in 1..quantity do
        {   personalName =
                if quantity > 1 then sprintf "%s %d" name i else name
                |> fun name -> if includeTeamInName then sprintf "%s %s" (renderTeam team) name else name
            team = team
            stats = db[name]
            damageTaken = 0
            statusMods = [] }
        ]
let createCombat (db: Map<string, Creature>) team1 team2 =
    { combatants =
        (team1 |> List.collect (toCombatants db 0 team2)) @ (team2 |> List.collect (toCombatants db 1 team1))
        |> Seq.map(fun c -> c.personalName, c)
        |> Map.ofSeq
        }
let db = Defaults.database()
(3, "Orc") |> toCombatants db 0 []
let combat = createCombat db [ 3, "Orc"; 1, "Slugbeast"; 1, "Skeleton"] [ 14, "Orc" ]
combat.combatants.Values |> Seq.sortBy(fun c -> c.team, c.personalName) |> Seq.map (fun c -> $"{c.personalName} [{renderTeam c.team}]") |> Seq.iter (printfn "%s")
