module POCTests
open Common
open Packrat
open Swensen.Unquote
open Expecto
open Domain.Random
let verify =
    let mutable counter = 0
    let addCounter label =
        counter <- counter + 1
        sprintf "%d) %s" counter label
    fun label condition -> testCase (addCounter label) <| fun _ -> Swensen.Unquote.Assertions.test condition
type 't prop = 't option
type DamageType = Cutting | Impaling | Crushing | Piercing | Other
type Creature = {
    name: string prop
    pluralName: string prop
    ST: int prop
    DX: int prop
    IQ: int prop
    HT: int prop
    HP: int prop
    Speed: int prop
    WeaponMaster: bool prop
    WeaponSkill: int prop
    Damage: RollSpec prop
    DamageType: DamageType prop
    }

let parseCreature (input: string): Creature = notImpl()
[<Tests>]
let Tests = testLabel "Acceptance Tests" <| testList "Creature Parsing" [
    let creature = lazy(
        parseCreature "Bob: ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut"
        )
    verify "Is named Bob" <@ creature.Value.name.Value = "Bob" @>
    verify "ST 17" <@ creature.Value.ST.Value = 17 @>
    verify "DX 12" <@ creature.Value.DX.Value = 12 @>
    ()
    ]
