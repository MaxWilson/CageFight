module Domain.Parse.Tests
open Expecto

open Packrat
open Swensen.Unquote
open Expecto
open Domain
open Domain.Parser
open Domain.Random
open Domain.Random.Parser

let makeVerify() =
    let mutable counter = 0
    let addCounter label =
        counter <- counter + 1
        sprintf "%d) %s" counter label
    let verify = fun label condition -> testCase (addCounter label) <| fun _ -> Swensen.Unquote.Assertions.test condition
    let pverify = fun label condition -> ptestCase (addCounter label) <| fun _ -> Swensen.Unquote.Assertions.test condition
    verify, pverify
// Helper to give better error messages on parse fail
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input

let parse (|Pattern|_|) input =
    match ParseArgs.Init input with
    | Pattern (v, End) -> v
    | v -> parseFail v

[<Tests>]
let UnitTests() = (testLabel "Unit tests" << testLabel "Domain") <| testList "Parse" [
    let verify, pverify = makeVerify()
    verify "Basic roll" <@ parse (|Roll|_|) "3d6" = RollSpec.create(3,6) @>
    verify "Basic roll" <@ parse (|Roll|_|) "3d-1" = RollSpec.create(3,6,-1) @>
    verify "Basic roll" <@ parse (|Roll|_|) "1d+1" = RollSpec.create(1,6,+1) @>
    verify "Basic roll" <@ parse (|Roll|_|) "4d" = RollSpec.create(4,6) @>
    verify "thr" <@ parse (|DamageOverall|_|) "thr" = (Thrust 0, None) @>
    verify "thr -1 imp" <@ parse (|DamageOverall|_|) "thr -1 imp" = (Thrust -1, Some Impaling) @>
    verify "thr-1" <@ parse (|DamageOverall|_|) "thr-1" = (Thrust -1, None) @>
    verify "sw+2 cut" <@ parse (|DamageOverall|_|) "sw+2 cut" = (Swing +2, Some Cutting) @>
    verify "4d-1 cr" <@ parse (|DamageOverall|_|) "4d-1 cr" = (Explicit (RollSpec.create(4,6,-1)), Some Crushing) @>
    ]

[<Tests>]
let AcceptanceTests() = testLabel "Acceptance tests" <| testList "Parsing" [
    let weaponStats (c: Creature) = c.WeaponSkill.Value, c.Damage_, defaultArg c.DamageType Other
    testList "Bob" [
        let verify, pverify = makeVerify()
        let creature = lazy(
            parse (|Creature|_|) "Bob the Barbarian: ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut"
            )
        verify "Is named Bob" <@ creature.Value.name = "Bob the Barbarian" @>
        verify "ST" <@ creature.Value.ST.Value = 17 @>
        verify "DX" <@ creature.Value.DX.Value = 12 @>
        verify "Skill, computed damage" <@ weaponStats creature.Value = (22, RollSpec.create(3,6,+7), Cutting)  @>
        ]
    testList "Tiger" [
        let verify, pverify = makeVerify()
        let creature = lazy(
            parse (|Creature|_|) "Tiger: ST 13 DX 15 IQ 11 HT 12 Skill 16 2d-1 imp"
            )
        verify "Name" <@ creature.Value.name = "Tiger" @>
        verify "ST" <@ creature.Value.ST.Value = 13 @>
        verify "DX" <@ creature.Value.DX.Value = 15 @>
        verify "Skill, computed damage" <@ weaponStats creature.Value = (16, RollSpec.create(2,6,-1), Impaling)  @>
        ]
    ]
