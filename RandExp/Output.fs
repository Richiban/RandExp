module Richiban.RandExp.Output

open Richiban.RandExp.Domain
open FParsec.CharParsers
open FParsec
open Domain

let random = System.Random()

let wordChars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray()

let digits = "1234567890".ToCharArray()

let symbols = "!@#$%^&*()_+-=[]{}<>?".ToCharArray()

let allChars =
    Array.concat [| wordChars
                    digits
                    symbols |]

let nonWordChars = Array.concat [| symbols |]

let nonDigits = Array.concat [| wordChars; symbols |]

let randomFrom (items: _ array) = items.[random.Next(items.Length)]

let executeSpecialChar =
    function
    | AnyChar -> randomFrom allChars
    | AnyWordChar -> randomFrom wordChars
    | AnyNonWordChar -> randomFrom nonWordChars
    | AnyWhitespaceChar -> ' '
    | AnyNonWhitespaceChar -> randomFrom allChars
    | AnyDigit -> randomFrom digits
    | AnyNonDigit -> randomFrom nonDigits

let rec executeTerm =
    function
    | CharLiteral c -> string c
    | NegativeCharSet items ->
        items
        |> Seq.map (function
            | Range (a, b) ->
                allChars
                |> Array.except { a .. b }
                |> randomFrom
                |> string
            | SingleItem c ->
                allChars
                |> Array.except [ c ]
                |> randomFrom
                |> string)
        |> String.concat ""
    | SpecialChar cx -> executeSpecialChar cx |> string
    | Count (a, b) -> executeMod a b
    | Group terms -> terms |> executeAll
    | RSet items ->
        items
        |> Seq.map (function
            | Range (a, b) -> { a .. b } |> Seq.map string |> String.concat ""
            | SingleItem c -> string c)
        |> String.concat ""
    | Union (left, right) as u ->
        let rec flatten terms =
            seq {
                match terms with
                | [| Union (left, right) |] ->
                    yield! flatten left
                    yield! flatten right
                | x -> x
            }

        let ts =
            Array.concat [| flatten left |> Array.ofSeq
                            flatten right |> Array.ofSeq |]

        printfn "---"
        printfn "%A" ts.Length
        printfn "%A" ts
        printfn "---"

        randomFrom ts |> executeAll

and executeAll (terms: Term seq) =
    terms |> Seq.map executeTerm |> String.concat ""

and executeMod (term: Term) =
    function
    | ExactCount i ->
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | MinCount m ->
        let i = random.Next(m, m + 10)
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | MaxCount m ->
        let i = random.Next(0, m)
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | RangeCount (a, b) ->
        let i = random.Next(a, b + 1)
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""

let execute (schema: RandomSchema) =
    let randomFrom =
        let random = System.Random()

        fun (items: _ array) -> items.[random.Next(items.Length)]

    schema
    |> Seq.map (executeTerm)
    |> String.concat ""
