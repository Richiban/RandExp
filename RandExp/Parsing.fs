module Richiban.RandExp.Parsing

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec

let k v _ = v

let specialChar: Parser<Term, unit> =
    choice [ skipChar '.' |>> k AnyChar
             skipString "\\s" |>> k AnyWhitespaceChar
             skipString "\\S" |>> k AnyNonWhitespaceChar
             skipString "\\w" |>> k AnyWordChar
             skipString "\\W" |>> k AnyNonWordChar
             skipString "\\d" |>> k AnyDigit
             skipString "\\D" |>> k AnyNonDigit ]
    |>> SpecialChar

let parseChar = (letter <|> digit) |>> CharLiteral

let private betweenChars opn close = between (skipChar opn) (skipChar close)

let parseCharSet =
    betweenChars '[' ']' (many1 (letter <|> digit))
    |>> (Array.ofList >> CharSet)

#nowarn "40"

let rec parseTerm =
    parse.Delay(fun () ->
        choice [ parseChar
                 parseCharSet
                 specialChar
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
                      (pint32 .>> skipChar '}' |>> ExactCount) ]
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
