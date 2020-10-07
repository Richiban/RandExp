module Richiban.RandExp.Parsing

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec

/// For debugging
let (<!>) (p: Parser<_, _>) label: Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let parseSpecialChar: Parser<Term, unit> =
    choice [ skipChar '.' >>% AnyChar
             skipString "\\s" >>% AnyWhitespaceChar
             skipString "\\S" >>% AnyNonWhitespaceChar
             skipString "\\w" >>% AnyWordChar
             skipString "\\W" >>% AnyNonWordChar
             skipString "\\d" >>% AnyDigit
             skipString "\\D" >>% AnyNonDigit ]
    |>> SpecialChar

let charDef =
    choice [ letter
             digit
             pchar ' '
             skipString "\\." >>% '.'
             skipString "\\^" >>% '^'
             skipString "\\$" >>% '$'
             skipString "\\*" >>% '*'
             skipString "\\+" >>% '+'
             skipString "\\?" >>% '?'
             skipString "\\(" >>% '('
             skipString "\\)" >>% ')'
             skipString "\\[" >>% '['
             skipString "\\{" >>% '{'
             skipString "\\\\" >>% '\\'
             skipString "\\|" >>% '|' ]

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
                 skipChar '*' >>% (MinCount 0 |> Count)
                 skipChar '+' >>% (MinCount 1 |> Count)
                 skipChar '?' >>% (RangeCount(0, 1) |> Count) ]

    parseTerm .>>. countForms |>> Mod

let parseSpec =
    ((attempt parseCount) <|> parseTerm) |> many

let parseSpecFull = spaces >>. parseSpec .>> spaces .>> eof

let parse input =
    match run parseSpecFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
