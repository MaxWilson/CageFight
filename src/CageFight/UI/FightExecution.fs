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

module CombatEvents =
    type Event =
        | Hit of Ids * DefenseDetails * injury:int * Status list * string
        | SuccessfulDefense of Ids * DefenseDetails * string
        | Miss of Ids * string
        | FallUnconscious of CombatantId * string
        | Unstun of CombatantId * string
        | StandUp of CombatantId * string
        | Info of CombatantId * string
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

type CalibrationResult = {
    quantity: int
    winFraction: float
    sampleCombat: Combat
    }
type FightResult =
    | CalibratedResult of CalibrationResult
    | SpecificResult of Combat
