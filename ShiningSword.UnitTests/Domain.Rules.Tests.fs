module Domain.Rules.Tests
open Expecto
open Domain
open Domain.Random
open Domain.FightExecution
let verify = Swensen.Unquote.Assertions.test

// Slightly simpler version of DefenseDetails to make test output easier to read
type DefenseResult = { defense: DefenseType; targetRetreated: bool }
    with static member create (targetNumber, input: DefenseDetails) = targetNumber, { defense = input.defense; targetRetreated = input.retreatFrom.IsSome }

[<Tests>]
let Tests = testLabel "Unit" <| testList "Rules" [
    testCase "Spot check damage computations" <| fun () ->
        verify <@ swingDamage 25 +2 = RollSpec.create(5,6,+1) @>
        verify <@ swingDamage 6 +2 = RollSpec.create(1,6,-1) @>
        verify <@ thrustDamage 3 0 = RollSpec.create(1,6,-5) @>
        verify <@ thrustDamage 37 +3 = RollSpec.create(4,6,+3) @>
        let baseDamage st =
            thrustDamage st 0, swingDamage st 0
        verify <@ baseDamage 16 = (RollSpec.create(1,6,+1), RollSpec.create(2,6,+2)) @>
        verify <@ baseDamage 45 = (RollSpec.create(5,6), RollSpec.create(7,6,+1)) @>
        verify <@ baseDamage 70 = (RollSpec.create(8,6), RollSpec.create(10,6)) @>
        verify <@ baseDamage 75 = (RollSpec.create(8,6,+2), RollSpec.create(10,6,+2)) @>
        verify <@ baseDamage 100 = (RollSpec.create(11,6), RollSpec.create(13,6)) @>

    testCase "Spot check defense choice" <| fun () ->
        let previousAttacker = (2, "Ogre 1")
        let attacker = (2, "Ogre 2")
        let create dodge parry block retreatUsed =
            let stats = { Creature.create("test") with Dodge = Some dodge; Parry = Some parry; Block = Some block }
            { Combatant.fresh(1, "test1", 1, stats) with retreatUsed = if retreatUsed then Some previousAttacker else None }
        let chooseDefenseWith f dodge parry block retreat parriesUsed =
            let combatant = create dodge parry block retreat
            let combatant = { combatant with parriesUsed = parriesUsed; stats = f combatant.stats }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefenseWithExtraParry dodge parry block retreat extraParries parriesUsed =
            chooseDefenseWith (fun stats -> { stats with ExtraParry = Some extraParries }) dodge parry block retreat parriesUsed
        let chooseDefenseWithExtraParryF f dodge parry block retreat extraParries parriesUsed =
            chooseDefenseWith (fun stats -> { stats with ExtraParry = Some extraParries } |> f) dodge parry block retreat parriesUsed
        let chooseDefenseUnderConditions dodge parry block retreat damage conditions =
            let combatant = { create dodge parry block retreat with statusMods = conditions; injuryTaken = damage }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefenseWithPriorRetreat dodge parry block previousRetreat =
            let combatant = { create dodge parry block false with retreatUsed = Some previousRetreat }
            chooseDefense attacker combatant |> DefenseResult.create
        let chooseDefense dodge parry block retreat =
            let combatant = create dodge parry block retreat
            chooseDefense attacker combatant |> DefenseResult.create
        verify <@ chooseDefense 10 0 0 true = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefense 10 13 9 true = (13, { defense = Parry; targetRetreated = false }) @>
        verify <@ chooseDefense 10 9 14 true = (14, { defense = Block; targetRetreated = false }) @>
        verify <@ chooseDefense 10 10 10 false = (13, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefense 10 13 9 false = (14, { defense = Parry; targetRetreated = true }) @>
        verify <@ chooseDefense 10 9 14 false = (15, { defense = Block; targetRetreated = true }) @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 7 [] = (8, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 10 10 false 7 [] = (11, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned] = (6, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 0 [Stunned] = (6, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 7 [Stunned] = (1, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 7 [Stunned] = (1, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 0 [Prone] = (10, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned;Prone] = (3, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 false 5 0 = (14, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 0 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 1 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 5 = (13, { defense = Parry; targetRetreated = false })  @>
        let chooseDefenseWithFencingParry = chooseDefenseWithExtraParryF (fun s -> { s with FencingParry = true })
        verify <@ chooseDefenseWithFencingParry 10 13 0 true 5 6 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 6 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 7 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 13 0 true 5 7 = (9, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 17 14 true 0 3 = (14, { defense = Block; targetRetreated = false })  @>
        let chooseDefenseWeaponMasterFencing args = chooseDefenseWithExtraParryF (fun s -> { s with WeaponMaster = true; FencingParry = true }) args
        let chooseDefenseWeaponMasterBroadsword args = chooseDefenseWithExtraParryF (fun s -> { s with WeaponMaster = true }) args
        verify <@ chooseDefenseWithExtraParry 10 17 0 true 0 3 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithFencingParry 10 17 0 true 0 3 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterBroadsword 10 17 0 true 0 3 = (11, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterFencing 10 17 0 true 0 3 = (14, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWeaponMasterFencing 10 17 0 false 0 3 = (17, { defense = Parry; targetRetreated = true })  @>
        // prove that retreat works across multiple defenses
        verify <@ chooseDefenseWithPriorRetreat 10 0 0 previousAttacker = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithPriorRetreat 10 0 0 attacker = (13, { defense = Dodge; targetRetreated = true })  @>

    testCase "Spot check target prioritization" <| fun () ->
        let attacker =
            Combatant.fresh(1, "Andy", 1, Creature.create "Knight")
        let mutable counter = 2
        let create name injury conditions =
            { Combatant.fresh(2, name, counter, Creature.create "Target") with injuryTaken = injury; statusMods = conditions }
        // we put stunned and prone targets at high priority.
        // prefer targets that are stunned but not yet at -HP,
        // then targets that are prone but not yet at -HP,
        // then targets that are below 1/3 HP but not yet at 0 HP,
        // then targets at or below 0 HP
        // then anyone still alive (ordered by statblock name and number because why not, and it makes readouts more predictable)
        let combat =
            [
                attacker
                create "Perfectly Fine Guy" 0 []
                create "Dead Guy" 100 [Dead]
                create "Stunned Guy" 3 [Stunned]
                create "Prone Guy" 3 [Prone]
                create "Stunned Dying Guy" 23 [Stunned]
                create "Hurt Guy" 8 []
                create "Badly Hurt Guy" 13 []
                create "Dying Guy" 22 [Unconscious]
                ]
            |> fun guys -> { combatants = guys |> List.map (fun c -> c.Id, c) |> Map.ofList }
        let priority = prioritizeTargets combat attacker |> List.ofSeq |> List.map (fun c -> c.personalName)
        verify <@ priority
                    = ["Stunned Guy"; "Prone Guy"; "Hurt Guy"; "Perfectly Fine Guy"; "Badly Hurt Guy"; "Stunned Dying Guy"] @>
    testCase "Spot check death check thresholds" <| fun () ->
        let getThresholds fullHP startFrom =
            let mutable thresholds = []
            failedDeathcheck (fun threshold -> thresholds <- thresholds@[threshold]; true) fullHP startFrom (fullHP * -5 + 1)
            |> ignore
            thresholds
        verify <@ getThresholds 10 0 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 10 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 -9 = [-10; -20; -30; -40] @>
        verify <@ getThresholds 10 -10 = [-20; -30; -40] @>
        verify <@ getThresholds 14 -10 = [-14; -28; -42; -56] @>
    ]
