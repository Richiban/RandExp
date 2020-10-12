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
    let parseRange =
        attempt (charDef .>> skipChar '-' .>>. charDef |>> Range)

    let parseSingle = charDef |>> SingleItem

    attempt
        (between (skipString "[^") (skipChar ']') (many1 (parseRange <|> parseSingle))
         |>> (Array.ofList >> NegativeCharSet))
    <|> (betweenChars '[' ']' (many1 (parseRange <|> parseSingle))
         |>> (Array.ofList >> RSet))

#nowarn "40"

let rec parseElement =
    parse.Delay(fun () ->
        choice [ parseCharLiteral
                 parseCharSet
                 parseSpecialChar
                 parseGroup ])

and parseGroup =
    betweenChars '(' ')' (many1 parseTerm)
    |>> (Array.ofList >> Group)

and parseCount =
    let parseBetweenBraces =
        betweenChars
            '{'
            '}'
            (choice [ attempt (skipChar ',' >>. pint32 |>> MaxCount)
                      attempt (pint32 .>> skipChar ',' .>>. pint32 |>> RangeCount)
                      attempt (pint32 .>> skipChar ',' |>> MinCount)
                      (pint32 |>> ExactCount) ])

    choice [ parseBetweenBraces
             skipChar '*' >>% (MinCount 0)
             skipChar '+' >>% (MinCount 1)
             skipChar '?' >>% (RangeCount(0, 1)) ]

and parseTerm =
    parseElement
    .>>. opt parseCount
    |>> function
    | element, None -> element
    | element, Some count -> Count(element, count)

let parseTerms = many parseCount .>> eof
let parseSpec = many parseTerm

let parseSpecFull = spaces >>. parseSpec .>> spaces .>> eof

let parse input =
    match run parseSpecFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
