module Domain.Rules.Tests
open Expecto
open Domain
open Domain.Random
open Domain.FightExecution
let verify = Swensen.Unquote.Assertions.test

[<Tests>]
let Tests = testLabel "Domain" <| testList "Rules" [
    testCase "Spot check damage computations" <| fun () ->
        verify <@ swingDamage 25 +2 = RollSpec.create(5,6,+1) @>
        verify <@ swingDamage 6 +2 = RollSpec.create(1,6,-1) @>
        verify <@ thrustDamage 3 0 = RollSpec.create(1,6,-5) @>
        verify <@ thrustDamage 37 +3 = RollSpec.create(4,6,+3) @>
    testCase "Spot check defense choice" <| fun () ->
        let create dodge parry block retreat =
            let stats = { Creature.create("test") with Dodge = Some dodge; Parry = Some parry; Block = Some block }
            { Combatant.fresh(1, "test1", 1, stats) with retreatUsed = retreat }
        let chooseDefenseWithExtraParry dodge parry block retreat extraParries parriesUsed =
            let combatant = create dodge parry block retreat
            let combatant = { combatant with stats = { combatant.stats with ExtraParry = Some extraParries }; parriesUsed = parriesUsed }
            chooseDefense combatant
        let chooseDefenseUnderConditions dodge parry block retreat damage conditions =
            let combatant = { create dodge parry block retreat with statusMods = conditions; injuryTaken = damage }
            chooseDefense combatant
        let chooseDefense dodge parry block retreat =
            let combatant = create dodge parry block retreat
            chooseDefense combatant
        verify <@ chooseDefense 10 0 0 true = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefense 10 13 9 true = (13, { defense = Parry; targetRetreated = false }) @>
        verify <@ chooseDefense 10 9 14 true = (14, { defense = Block; targetRetreated = false }) @>
        verify <@ chooseDefense 10 10 10 false = (13, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefense 10 13 9 false = (14, { defense = Parry; targetRetreated = true }) @>
        verify <@ chooseDefense 10 9 14 false = (15, { defense = Block; targetRetreated = true }) @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 7 [] = (8, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 10 10 false 7 [] = (11, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned] = (6, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 7 [Stunned] = (1, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 false 0 [Prone] = (10, { defense = Dodge; targetRetreated = true })  @>
        verify <@ chooseDefenseUnderConditions 10 0 0 true 0 [Stunned;Prone] = (3, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 false 5 0 = (14, { defense = Parry; targetRetreated = true })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 0 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 1 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 5 = (13, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 6 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 10 13 0 true 5 7 = (10, { defense = Dodge; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 13 0 true 5 7 = (9, { defense = Parry; targetRetreated = false })  @>
        verify <@ chooseDefenseWithExtraParry 0 17 14 true 0 3 = (14, { defense = Block; targetRetreated = false })  @>

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
                create "Dying Guy" 22 [Unconscious]
                ]
            |> fun guys -> { combatants = guys |> List.map (fun c -> c.Id, c) |> Map.ofList }
        let priority = prioritizeTargets combat attacker |> List.ofSeq |> List.map (fun c -> c.personalName)
        verify <@ priority
                    = ["Stunned Guy"; "Prone Guy"; "Hurt Guy"; "Stunned Dying Guy"; "Perfectly Fine Guy"] @>

    ]
