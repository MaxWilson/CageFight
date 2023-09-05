module POCTests
open Common
open Packrat
open Swensen.Unquote
open Expecto
open Domain.Random
let makeVerify() =
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
let weaponStats (c: Creature) = c.WeaponSkill.Value, c.Damage.Value, c.DamageType.Value

let parseCreature (input: string): Creature =
    { name = Some "Bob the Barbarian"
      pluralName = None
      ST = None
      DX = None
      IQ = None
      HT = None
      HP = None
      Speed = None
      WeaponMaster = None
      WeaponSkill = Some 22
      Damage = Some <| RollSpec.create(3,6,+7)
      DamageType = Some Cutting
      }
[<Tests>]
let Tests = testLabel "Acceptance Tests" <| testList "Creature Parsing" [
    testList "Bob" [
        let verify = makeVerify()
        let creature = lazy(
            parseCreature "Bob: ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut"
            )
        verify "Is named Bob" <@ creature.Value.name.Value = "Bob the Barbarian" @>
        verify "ST" <@ creature.Value.ST.Value = 17 @>
        verify "DX" <@ creature.Value.DX.Value = 12 @>
        verify "Skill, computed damage" <@ weaponStats creature.Value = (22, RollSpec.create(3,6,+7), Cutting)  @>
        ]
    testList "Tiger" [
        let verify = makeVerify()
        let creature = lazy(
            parseCreature "Tiger: ST 13 DX 15 IQ 11 HT 12 Skill 16 2d-1 imp"
            )
        verify "Name" <@ creature.Value.name.Value = "Tiger" @>
        verify "ST" <@ creature.Value.ST.Value = 13 @>
        verify "DX" <@ creature.Value.DX.Value = 15 @>
        verify "Skill, computed damage" <@ weaponStats creature.Value = (16, RollSpec.create(2,6,-1), Impaling)  @>
        ]
    ]
