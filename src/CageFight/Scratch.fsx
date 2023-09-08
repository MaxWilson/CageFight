#load @"Core\Common.fs"
#load @"Core\Packrat.fs"
#load @"Domain\Random.fs"
#load @"Domain\Domain.fs"
open Domain.Random
let swingDamage st bonusOrPenalty =
    if st < 9 then RollSpec.create(1,6, bonusOrPenalty + (st-12)/2)
    elif st < 28 then
        let nDice = 1 + ((st-9) / 4)
        let bonus = (st-9) % 4 - 1
        RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
    else
        notImpl "Swing damage for ST 28+"
for st in [1..27] do
    printfn "%d: %O" st (swingDamage st 0)
let thrustDamage st bonusOrPenalty =
    if st < 13 then RollSpec.create(1,6, bonusOrPenalty + (st-14)/2)
    elif st <= 40 then
        let nDice = 1 + (st-11) / 8
        let bonus = (st-11) / 2 % 4 - 1
        RollSpec.create(nDice, 6, bonusOrPenalty + bonus)
    else
        notImpl "Thrust damage for ST 28+"
for st in [1..40] do
    printfn "%d: %O" st (thrustDamage st 0)

open Domain.Parser
open Packrat
match ParseArgs.Init "Bob the Barbarian: ST 14" with
| Creature(c, End) -> c
