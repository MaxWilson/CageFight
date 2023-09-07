module Domain
open Expecto

open Packrat
open Swensen.Unquote
open Expecto
open Domain
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
let Tests() = testLabel "Domain" <| testList "Parse" [
    let verify, pverify = makeVerify()
    verify "Basic roll" <@ parse (|Roll|_|) "3d6" = RollSpec.create(3,6) @>
    verify "Basic roll" <@ parse (|Roll|_|) "3d-1" = RollSpec.create(3,6,-1) @>
    verify "Basic roll" <@ parse (|Roll|_|) "1d+1" = RollSpec.create(1,6,+1) @>
    verify "Basic roll" <@ parse (|Roll|_|) "4d" = RollSpec.create(4,6) @>
    ]
