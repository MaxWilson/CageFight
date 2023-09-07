[<AutoOpen>]
module Domain.Core
open Domain.Random

type 't prop = 't option
type DamageType = Cutting | Impaling | Crushing | Piercing | Other
type Creature = {
    name: string
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
    with
    static member create (name: string, ?pluralName) =
        { name = name
          pluralName = pluralName
          ST = None
          DX = None
          IQ = None
          HT = None
          HP = None
          Speed = None
          WeaponMaster = None
          WeaponSkill = None
          Damage = None
          DamageType = None
          }

type MonsterDatabase = {
    catalog: Map<string, Creature>
    }
    with
    static member fresh = { catalog = Map.empty }
    static member add (monster: Creature) (db: MonsterDatabase) =
        { db with catalog = Map.add monster.name monster db.catalog }
