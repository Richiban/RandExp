module Richiban.RandExp.Parsing1

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec

let k v _ = v

let ws : Parser<_, unit> = skipMany (skipChar ' ')
let atLeast1ws: Parser<_, unit> = skipMany1 (skipChar ' ')

let specialChar: Parser<Term, unit>  =
    choice [ skipChar '.' |>> k AnyChar
             skipString "\\w" |>> k AnyWhitespaceChar ]
    |>> SpecialChar

let parseChar = letter |>> CharLiteral
let parseCharSet = skipChar '[' >>. (many1 (letter <|> digit)) .>> skipChar ']'
                   |>> CharSet

let parseTerm =
    choice [ parseChar
             parseCharSet
             specialChar ]

let parseCount = 
    parseTerm
    .>> skipChar '{' 
    .>>. (choice [ skipChar ',' >>. pint32 .>> skipChar '}' |>> MaxCount 
                   pint32 .>> skipChar ',' .>>. pint32 .>> skipChar '}' |>> RangeCount
                   pint32 .>> skipChar ',' .>> skipChar '}' |>> MinCount 
                   pint32 .>> skipChar '}' |>> ExactCount ] 
          |>> Count)
    |>> Mod

let parseSpec =
    (parseCount <|> parseTerm)
    |> many

let parseSpecFull = spaces >>. parseSpec .>> spaces .>> eof

let parse input =
    match run parseSpecFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
