module Domain.Rules.Tests
open Expecto
open Domain
open Domain.Random
let verify = Swensen.Unquote.Assertions.test

let Tests = testLabel "Domain" <| testList "Rules" [
    testCase "Spot check damage computations" <| fun () ->
        verify <@ swingDamage 25 +2 = RollSpec.create(5,6,-1) @>
        verify <@ swingDamage 6 +2 = RollSpec.create(1,6,-1) @>
        verify <@ thrustDamage 3 0 = RollSpec.create(1,6,-5) @>
        verify <@ thrustDamage 37 +3 = RollSpec.create(4,6,+3) @>
    ]
