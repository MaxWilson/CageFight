namespace Domain

[<AutoOpen>]
module Core =
    open Domain.Random

    type 't prop = 't option
    type DamageType = Cutting | Impaling | Crushing | Piercing | Burning | Other
    type DamageSpec = Explicit of RollSpec | Swing of int | Thrust of int
    let ticksToDice ticks bonusOrPenalty =
        // every 4 dice is +1d.
        if ticks % 4 = 3 then
            RollSpec.create((ticks + 1)/4, 6, -1 + bonusOrPenalty)
        else
            RollSpec.create(ticks / 4, 6, (ticks % 4) + bonusOrPenalty)
    // Swing is (ST-6)/4 dice up to 26 (5d), then (ST+14)/8d dice up to 50 (8d), then (ST+30)/10 dice.
    // Thrust is (ST-6)/8 dice (rounded up to the next 0.25 dice) up to 70 (8d), then (ST+10)/10.
    let swingDamage st bonusOrPenalty =
        if st < 9 then RollSpec.create(1,6, bonusOrPenalty + (st-12)/2)
        elif st < 26 then ticksToDice (st-6) bonusOrPenalty
        elif st <= 50 then ticksToDice ((st+14) / 2) bonusOrPenalty
        else ticksToDice ((st+30)*4/10) bonusOrPenalty
    let thrustDamage st bonusOrPenalty =
        if st < 13 then RollSpec.create(1,6, bonusOrPenalty + (st-14)/2)
        elif st <= 70 then ticksToDice ((st-6+1) / 2) bonusOrPenalty
        else ticksToDice ((st+10+1)*4/10) bonusOrPenalty
    type InjuryTolerance = Unliving | Homogeneous | Diffuse
    type Creature = {
        name: string
        pluralName: string prop
        ST: int prop
        DX: int prop
        IQ: int prop
        HT: int prop
        HP: int prop
        DR: int prop
        Dodge: int prop
        Parry: int prop
        FencingParry: bool
        Block: int prop
        ExtraAttack: int prop
        ExtraParry: int prop
        Speed: float prop
        WeaponMaster: bool
        WeaponSkill: int prop
        Damage: DamageSpec prop
        DamageType: DamageType prop
        FollowupDamage: RollSpec prop
        FollowupDamageType: DamageType prop
        UnnaturallyFragile: bool
        SupernaturalDurability: bool
        InjuryTolerance: InjuryTolerance option
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
              FollowupDamage = None
              FollowupDamageType = None
              DR = None
              Dodge = None
              Parry = None
              FencingParry = false
              Block = None
              ExtraAttack = None
              ExtraParry = None
              UnnaturallyFragile = false
              SupernaturalDurability = false
              InjuryTolerance = None
              }
        // "_" means "defaulted" in the sense that it's the value that will be used if the property is not set.
        member this.PluralName_ = defaultArg this.pluralName (this.name + "s")
        member this.ST_ = defaultArg this.ST 10
        member this.DX_ = defaultArg this.DX 10
        member this.IQ_ = defaultArg this.IQ 10
        member this.HT_ = defaultArg this.HT 10
        member this.HP_ = defaultArg this.HP this.ST_
        member this.Speed_ = defaultArg this.Speed ((this.DX_ + this.HT_ |> float) / 4.)
        member this.DR_ = defaultArg this.DR 0
        member this.Dodge_ = defaultArg this.Dodge ((this.Speed_ |> int) + 3)
        // notice: Parry and Block do not exist by default
        member this.ExtraAttack_ = defaultArg this.ExtraAttack 0
        member this.ExtraParry_ = defaultArg this.ExtraParry 0
        member this.WeaponSkill_ = defaultArg this.WeaponSkill 10
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
            { catalog = Map.add monster.name monster db.catalog }

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
        | OWSStr "burning" rest
        | OWSStr "burn" rest -> Some(DamageType.Burning, rest)
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
        | OWSStr "DR" (Int (v, rest)) -> Some((fun c -> { c with DR = Some v }), rest)
        | OWSStr "Speed" (Decimal (v, rest)) -> Some((fun c -> { c with Speed = Some v }), rest)
        | OWSStr "Dodge" (Int (v, rest)) -> Some((fun c -> { c with Dodge = Some v }), rest)
        | OWSStr "Parry" (Int (v, Str "F" rest)) -> Some((fun c -> { c with Parry = Some v; FencingParry = true }), rest)
        | OWSStr "Parry" (Int (v, rest)) -> Some((fun c -> { c with Parry = Some v }), rest)
        | OWSStr "Block" (Int (v, rest)) -> Some((fun c -> { c with Block = Some v }), rest)
        | OWSStr "Weapon Master" rest -> Some((fun c -> { c with WeaponMaster = true }), rest)
        | OWSStr "Skill" (Int (v, rest)) -> Some((fun c -> { c with WeaponSkill = Some v }), rest)
        | DamageOverall((damage, damageType), OWSStr "+" (OWSStr "followup" (OWS (Roll((followupDamage, OptionalDamageType(followupDamageType, rest)))))))
            -> Some((fun c -> { c with Damage = Some damage; DamageType = damageType; FollowupDamage = Some followupDamage; FollowupDamageType = followupDamageType }), rest)
        | DamageOverall((damage, damageType), rest) -> Some((fun c -> { c with Damage = Some damage; DamageType = damageType }), rest)
        | OWSStr "Extra Attack" (Int (v, rest)) -> Some((fun c -> { c with ExtraAttack = Some v }), rest)
        | OWSStr "Extra Parry" (Int (v, rest)) -> Some((fun c -> { c with ExtraParry = Some v }), rest)
        | OWSStr "Unliving" rest -> Some((fun c -> { c with InjuryTolerance = Some Unliving }), rest)
        | OWSStr "Homogeneous" rest -> Some((fun c -> { c with InjuryTolerance = Some Homogeneous }), rest)
        | OWSStr "Diffuse" rest -> Some((fun c -> { c with InjuryTolerance = Some Diffuse }), rest)
        | OWSStr "Supernatural Durability" rest -> Some((fun c -> { c with SupernaturalDurability = true }), rest)
        | OWSStr "Unnatural" rest -> Some((fun c -> { c with UnnaturallyFragile = true }), rest)
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

module Defaults =
    open Packrat
    let database() =
        [
            let parse (input: string) =
                match ParseArgs.Init input with
                | Parser.Creature(c, End) -> c
                | _ -> shouldntHappen input
            parse "Peshkali [Peshkalir]: ST 20 DX 12 HT 12 DR 4 Skill 18 sw+1 cut Extra Attack 5 Extra Parry 5 Parry 13 Dodge 10 Supernatural Durability"
            parse "Orc: ST 12 DX 11 IQ 9 HT 11 DR 2 HP 14 Dodge 7 Parry 9 Block 9 Skill 13 sw+1 cut"
            parse "Ogre: ST 20 DX 11 IQ 7 HT 13 Skill 16 3d+7 cr Parry 11 DR 3"
            parse "Slugbeast: ST 16 IQ 2 Skill 12 1d+2 Homogeneous"
            parse "Skeleton: ST 11 DX 13 IQ 8 HT 12 Skill 14 1d+3 imp DR 2 Speed 8 Parry 10 Block 10 Unliving Unnatural"
            parse "Stone Golem: ST 20 DX 11 IQ 8 HT 14 HP 30 Parry 9 DR 4 Homogeneous Skill 13 sw+4 cut Unnatural"
            parse "Rock Mite: ST 12 HT 13 Speed 5.00 DR 5 Homogeneous Skill 10 1d-1 cut + followup 2d burn"
            parse "Inigo Montoya: ST 13 DX 16 IQ 11 HT 12 Speed 8.5 Dodge 12 Parry 17F DR 1 Weapon Master Skill 22 thr+2 imp Extra Attack 1"
            ]
        |> List.map(fun c -> c.name, c)
        |> Map.ofList
