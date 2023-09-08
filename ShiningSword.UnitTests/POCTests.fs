module POCTests
open Common
open Packrat
open Swensen.Unquote
open Expecto
open Domain
open Domain.Random
let makeVerify() =
    let mutable counter = 0
    let addCounter label =
        counter <- counter + 1
        sprintf "%d) %s" counter label
    let verify = fun label condition -> testCase (addCounter label) <| fun _ -> Swensen.Unquote.Assertions.test condition
    let pverify = fun label condition -> ptestCase (addCounter label) <| fun _ -> Swensen.Unquote.Assertions.test condition
    verify, pverify
let weaponStats (c: Creature) = c.WeaponSkill.Value, c.Damage_, defaultArg c.DamageType Other

let parseCreature (input: string): Creature =
    {   Creature.create "Bob the Barbarian" with
            WeaponSkill = Some 22
            Damage = RollSpec.create(3,6,+7) |> Explicit |> Some
            DamageType = Some Cutting
        }
[<Tests>]
let Tests = testLabel "Acceptance Tests" <| testList "Creature Parsing" [
    testList "Bob" [
        let verify, pverify = makeVerify()
        let creature = lazy(
            parseCreature "Bob: ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut"
            )
        verify "Is named Bob" <@ creature.Value.name = "Bob the Barbarian" @>
        pverify "ST" <@ creature.Value.ST.Value = 17 @>
        pverify "DX" <@ creature.Value.DX.Value = 12 @>
        verify "Skill, computed damage" <@ weaponStats creature.Value = (22, RollSpec.create(3,6,+7), Cutting)  @>
        ]
    testList "Tiger" [
        let verify, pverify = makeVerify()
        let creature = lazy(
            parseCreature "Tiger: ST 13 DX 15 IQ 11 HT 12 Skill 16 2d-1 imp"
            )
        pverify "Name" <@ creature.Value.name = "Tiger" @>
        pverify "ST" <@ creature.Value.ST.Value = 13 @>
        pverify "DX" <@ creature.Value.DX.Value = 15 @>
        pverify "Skill, computed damage" <@ weaponStats creature.Value = (16, RollSpec.create(2,6,-1), Impaling)  @>
        ]
    ]
