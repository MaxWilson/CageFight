namespace Domain

[<AutoOpen>]
module Core =
    open Domain.Random

    type 't prop = 't option
    type DamageType = Cutting | Impaling | Crushing | Piercing | Other
    type DamageSpec = Explicit of RollSpec | Swing of int | Thrust of int
    let swingDamage st bonusOrPenalty =
        if st < 9 then RollSpec.create(1,6, bonusOrPenalty + (st-12)/2)
        elif st < 28 then
            let nDice = 1 + ((st-9) / 4)
            let bonus = (st-9) % 4 - 1
            RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
        else
            notImpl "Swing damage for ST 28+"
    for st in [1..27] do
        printfn "%d: %O" st (swingDamage 0 st)
    let thrustDamage st bonusOrPenalty =
        if st < 13 then RollSpec.create(1,6, bonusOrPenalty + (st-14)/2)
        elif st <= 40 then
            let nDice = 1 + (st-11) / 8
            let bonus = (st-11) / 2 % 4 - 1
            RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
        else
            notImpl "Thrust damage for ST 28+"
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
            let addWeaponMasterDamage = function
                | RollSpec(n, d, rest) as roll when this.WeaponMaster ->
                    RollSpec.create(n*2) + roll
                | otherwise -> otherwise
            match defaultArg this.Damage (Thrust 0) with
            | Explicit roll -> roll
            | Swing bonusOrPenalty -> swingDamage this.ST_ bonusOrPenalty |> addWeaponMasterDamage
            | Thrust bonusOrPenalty -> thrustDamage this.ST_ bonusOrPenalty |> addWeaponMasterDamage

    type MonsterDatabase = {
        catalog: Map<string, Creature>
        }
        with
        static member fresh = { catalog = Map.empty }
        static member add (monster: Creature) (db: MonsterDatabase) =
            { db with catalog = Map.add monster.name monster db.catalog }

#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing
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
        | OWSStr "swing" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Swing bonusOrPenalty, dt), rest)
        | OWSStr "sw" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Swing bonusOrPenalty, dt), rest)
        | OWSStr "thrust" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Thrust bonusOrPenalty, dt), rest)
        | OWSStr "thr" (OptionalIntMod(bonusOrPenalty, OptionalDamageType(dt, rest))) -> Some((Thrust bonusOrPenalty, dt), rest)
        | _ -> None
    let (|CreatureProperty|_|) =
        pack <| function
        // e.g. ST 17 DX 12 IQ 9 HT 11 HP 22 Speed 14 Weapon Master Skill 22 sw+2 cut
        | OWSStr "ST" (Int (v, rest)) -> Some((fun c -> { c with ST = Some v }), rest)
        | OWSStr "DX" (Int (v, rest)) -> Some((fun c -> { c with DX = Some v }), rest)
        | OWSStr "IQ" (Int (v, rest)) -> Some((fun c -> { c with IQ = Some v }), rest)
        | OWSStr "HT" (Int (v, rest)) -> Some((fun c -> { c with HT = Some v }), rest)
        | OWSStr "HP" (Int (v, rest)) -> Some((fun c -> { c with HP = Some v }), rest)
        | OWSStr "Speed" (Int (v, rest)) -> Some((fun c -> { c with Speed = Some v }), rest)
        | OWSStr "Weapon Master" rest -> Some((fun c -> { c with WeaponMaster = true }), rest)
        | OWSStr "Skill" (Int (v, rest)) -> Some((fun c -> { c with WeaponSkill = Some v }), rest)
        | DamageOverall((damage, damageType), rest) -> Some((fun c -> { c with Damage = Some damage; DamageType = damageType }), rest)
        | _ -> None
    let rec (|CreatureProperties|_|) = pack <| function
        | CreatureProperties(fprops, CreatureProperty(fprop, rest)) -> Some(fprops >> fprop, rest)
        | CreatureProperty(fprop, rest) ->
            Some(fprop, rest)
        | _ -> None
    let (|Creature|_|) = pack <| function
        | Words(name, OWSStr "[" (Words(plural, OWSStr "]" (OWSStr ":" (CreatureProperties(fprops, rest)))))) ->
            Some(Creature.create(name, plural) |> fprops, rest)
        | Words(name, OWSStr ":" (CreatureProperties(fprops, rest))) ->
            Some(Creature.create(name) |> fprops, rest)
        | _ -> None
    let (|Command|_|) = pack <| function
        | Str "add" (Creature(monster, rest)) -> Some((fun db -> MonsterDatabase.add monster db), rest)
        | _ -> None
