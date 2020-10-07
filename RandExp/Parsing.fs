module Richiban.RandExp.Parsing

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec

/// K-combinator. A function that takes a value and turns it into a function that returns that value
let k v _ = v

/// For debugging
let (<!>) (p: Parser<_, _>) label: Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let parseSpecialChar: Parser<Term, unit> =
    choice [ skipChar '.' |>> k AnyChar
             skipString "\\s" |>> k AnyWhitespaceChar
             skipString "\\S" |>> k AnyNonWhitespaceChar
             skipString "\\w" |>> k AnyWordChar
             skipString "\\W" |>> k AnyNonWordChar
             skipString "\\d" |>> k AnyDigit
             skipString "\\D" |>> k AnyNonDigit ]
    |>> SpecialChar

let charDef =
    choice [ letter
             digit
             pchar ' '
             skipString "\\." |>> k '.'
             skipString "\\^" |>> k '^'
             skipString "\\$" |>> k '$'
             skipString "\\*" |>> k '*'
             skipString "\\+" |>> k '+'
             skipString "\\?" |>> k '?'
             skipString "\\(" |>> k '('
             skipString "\\)" |>> k ')'
             skipString "\\[" |>> k '['
             skipString "\\{" |>> k '{'
             skipString "\\\\" |>> k '\\'
             skipString "\\|" |>> k '|' ]

let parseCharLiteral = charDef |>> CharLiteral

let private betweenChars opn close = between (skipChar opn) (skipChar close)

let parseCharSet =
    betweenChars '[' ']' (many1 charDef)
    |>> (Array.ofList >> CharSet)

#nowarn "40"

let rec parseTerm =
    parse.Delay(fun () ->
        choice [ parseCharLiteral
                 parseCharSet
                 parseSpecialChar
                 parseGroup ])

and parseGroup =
    betweenChars '(' ')' (many1 parseTerm)
    |>> (Array.ofList >> Group)

let parseCount =
    let parseBetweenBraces =
        betweenChars
            '{'
            '}'
            (choice [ attempt (skipChar ',' >>. pint32 |>> MaxCount)
                      attempt (pint32 .>> skipChar ',' .>>. pint32 |>> RangeCount)
                      attempt (pint32 .>> skipChar ',' |>> MinCount)
                      (pint32 |>> ExactCount) ]
             |>> Count)

    let countForms =
        choice [ parseBetweenBraces
                 skipChar '*' |>> (fun () -> MinCount 0 |> Count)
                 skipChar '+' |>> (fun () -> MinCount 1 |> Count) ]

    parseTerm .>>. countForms |>> Mod

let parseSpec =
    ((attempt parseCount) <|> parseTerm) |> many

let parseSpecFull = spaces >>. parseSpec .>> spaces .>> eof

let parse input =
    match run parseSpecFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
