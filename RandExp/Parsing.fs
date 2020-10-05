module Richiban.RandExp.Parsing

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec

let k v _ = v

let ws: Parser<_, unit> = skipMany (skipChar ' ')
let atLeast1ws: Parser<_, unit> = skipMany1 (skipChar ' ')

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

let parseCharSet =
    skipChar '['
    >>. (many1 (letter <|> digit))
    .>> skipChar ']'
    |>> (Array.ofList >> CharSet)

let parseTerm =
    choice [ parseChar
             parseCharSet
             specialChar ]

let parseCount =
    parseTerm
    .>> skipChar '{'
    .>>. (choice [ attempt (skipChar ',' >>. pint32 |>> MaxCount)
                   attempt (pint32 .>> skipChar ',' .>>. pint32 |>> RangeCount)
                   attempt (pint32 .>> skipChar ',' |>> MinCount)
                   (pint32 .>> skipChar '}' |>> ExactCount) ]
          |>> Count)
    .>> skipChar '}'
    |>> Mod

let parseSpec =
    ((attempt parseCount) <|> parseTerm) |> many

let parseSpecFull = spaces >>. parseSpec .>> spaces .>> eof

let parse input =
    match run parseSpecFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
