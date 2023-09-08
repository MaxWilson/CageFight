namespace Domain

[<AutoOpen>]
module Core =
    open Domain.Random

    type 't prop = 't option
    type DamageType = Cutting | Impaling | Crushing | Piercing | Other
    type DamageSpec = Explicit of RollSpec | Swing of int | Thrust of int
    let swingDamage bonusOrPenalty = RollSpec.create(1,6)
    let thrustDamage bonusOrPenalty = RollSpec.create(1,6)
    type Creature = {
        name: string
        pluralName: string prop
        ST: int prop
        DX: int prop
        IQ: int prop
        HT: int prop
        HP: int prop
        Speed: float prop
        WeaponMaster: bool
        WeaponSkill: int prop
        Damage: DamageSpec prop
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
              WeaponMaster = false
              WeaponSkill = None
              Damage = None
              DamageType = None
              }
        // "_" means "defaulted" in the sense that it's the value that will be used if the property is not set.
        member this.PluralName_ = defaultArg this.pluralName (this.name + "s")
        member this.ST_ = defaultArg this.ST 10
        member this.DX_ = defaultArg this.DX 10
        member this.IQ_ = defaultArg this.IQ 10
        member this.HT_ = defaultArg this.HT 10
        member this.HP_ = defaultArg this.HP this.ST_
        member this.Speed_ = defaultArg this.Speed ((this.DX_ + this.HT_ |> float) / 4.)
        member this.Damage_ =
            match defaultArg this.Damage (Thrust 0) with
            | Explicit roll -> roll
            | Swing bonusOrPenalty -> swingDamage bonusOrPenalty
            | Thrust bonusOrPenalty -> thrustDamage bonusOrPenalty

    type MonsterDatabase = {
        catalog: Map<string, Creature>
        }
        with
        static member fresh = { catalog = Map.empty }
        static member add (monster: Creature) (db: MonsterDatabase) =
            { db with catalog = Map.add monster.name monster db.catalog }

module Parser =
    open Packrat
    open Random
    open Random.Parser
    let (|DamageType|_|) = pack <| function
        | OWSStr "crushing" rest
        | OWSStr "cr" rest -> Some(DamageType.Crushing, rest)
        | OWSStr "cutting" rest
        | OWSStr "cut" rest -> Some(DamageType.Cutting, rest)
        | OWSStr "impaling" rest
        | OWSStr "imp" rest -> Some(DamageType.Impaling, rest)
        | OWSStr "piercing" rest
        | OWSStr "pierce" rest
        | OWSStr "pi" rest -> Some(DamageType.Piercing, rest)
        | _ -> None
    let (|OptionalDamageType|_|) = pack <| function
        | DamageType(dt, rest) -> Some(Some dt, rest)
        | rest -> Some(None, rest)
    let (|OptionalIntMod|_|) = pack <| function
        | OWS (IntModifier(bonusOrPenalty, rest)) -> Some(bonusOrPenalty, rest)
        | rest -> Some(0, rest)
    let (|DamageOverall|_|) = pack <| function
        | Roll(roll, OptionalDamageType(dt, rest)) -> Some((Explicit roll, dt), rest)
        | OWSStr "sw" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Swing bonusOrPenalty, dt), rest)
        | OWSStr "thr" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Thrust bonusOrPenalty, dt), rest)
        | _ -> None
