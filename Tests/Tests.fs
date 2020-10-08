module Tests

open System
open Xunit
open Richiban.RandExp.Domain
open Richiban.RandExp.Parsing

let private expectFailure input =
    match parse input with
    | Result.Ok _ -> Assert.False(true, "Expected failure, got success")
    | Result.Error _ -> Assert.True(true)

let private expectSuccess input (x: Term list) =
    match parse input with
    | Result.Ok y -> Assert.Equal<Term list>(x, y)
    | Result.Error e -> Assert.False(true, e)

[<Fact>]
let ``Invalid char (`` = expectFailure "("

[<Fact>]
let ``Invalid char )`` = expectFailure ")"

[<Fact>]
let ``Invalid char [`` () = expectFailure "["

[<Fact>]
let ``Invalid char ]`` () = expectFailure "]"

[<Fact>]
let ``Invalid char {`` () = expectFailure "{"

[<Fact>]
let ``Invalid char }`` () = expectFailure "}"

[<Fact>]
let ``Basic char literal a`` () = expectSuccess "a" [ CharLiteral 'a' ]

[<Fact>]
let ``Escaped char literal .`` () = expectSuccess "\." [ CharLiteral '.' ]

[<Fact>]
let ``Basic char with group (a)`` () =
    expectSuccess "(a)" [ Group [| CharLiteral 'a' |] ]

[<Fact>]
let ``IPv4 address`` () =
    expectSuccess
        "(\d{1,3}\.){3}(\d{1,3})"
        [ Count
            (Group [| Count(SpecialChar AnyDigit, RangeCount(1, 3))
                      CharLiteral '.' |],
             ExactCount 3)
          Group [| Count(SpecialChar AnyDigit, RangeCount(1, 3)) |] ]

[<Fact>]
let ``Wub wub wub`` () =
    expectSuccess
        "([Ww]ub){3}"
        [ Count
            (Group [| CharSet [| 'W'; 'w' |]
                      CharLiteral 'u'
                      CharLiteral 'b' |],
             ExactCount 3) ]
