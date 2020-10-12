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
let ``Count: any`` () =
    expectSuccess "a*" [ Count(CharLiteral 'a', MinCount(0)) ]

[<Fact>]
let ``Count: at least one`` () =
    expectSuccess "a+" [ Count(CharLiteral 'a', MinCount(1)) ]

[<Fact>]
let ``Count: exact number`` () =
    expectSuccess "a{2}" [ Count(CharLiteral 'a', ExactCount(2)) ]

[<Fact>]
let ``Count: minimum range`` () =
    expectSuccess "a{3,}" [ Count(CharLiteral 'a', MinCount(3)) ]

[<Fact>]
let ``Count: maximum range`` () =
    expectSuccess "a{,4}" [ Count(CharLiteral 'a', MaxCount(4)) ]

[<Fact>]
let ``Count: range`` () =
    expectSuccess "a{5,6}" [ Count(CharLiteral 'a', RangeCount(5, 6)) ]

[<Fact>]
let ``Charset: [abc]`` () =
    expectSuccess
        "[abc]"
        [ RSet [| SingleItem 'a'
                  SingleItem 'b'
                  SingleItem 'c' |] ]

[<Fact>]
let ``Negated Charset: [^abc]`` () =
    expectSuccess
        "[^abc]"
        [ NegativeCharSet [| SingleItem 'a'
                             SingleItem 'b'
                             SingleItem 'c' |] ]

[<Fact>]
let ``CharRange: [a-z]`` () =
    expectSuccess "[a-z]" [ RSet [| Range('a', 'z') |] ]

[<Fact>]
let ``CharRange: [a-zA-Z]`` () =
    expectSuccess
        "[a-zA-Z]"
        [ RSet [| Range('a', 'z')
                  Range('A', 'Z') |] ]

[<Fact>]
let ``CharRange: [^a-zA-Z]`` () =
    expectSuccess
        "[^a-zA-Z]"
        [ NegativeCharSet [| Range('a', 'z')
                             Range('A', 'Z') |] ]

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
            (Group [| RSet [| SingleItem 'W'
                              SingleItem 'w' |]
                      CharLiteral 'u'
                      CharLiteral 'b' |],
             ExactCount 3) ]
