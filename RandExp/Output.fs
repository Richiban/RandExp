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
    | CharSet cs -> randomFrom cs |> string
    | SpecialChar cx -> executeSpecialChar cx |> string
    | Mod (a, b) -> executeMod a b
    | Group terms -> String.concat "" (terms |> Seq.map executeTerm)

and executeMod (term: Term) =
    function
    | Count (ExactCount i) ->
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | Count (MinCount m) ->
        let i = random.Next(m, m + 10)
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | Count (MaxCount m) ->
        let i = random.Next(0, m)
        Seq.init i (fun _ -> executeTerm term)
        |> String.concat ""
    | Count (RangeCount (a, b)) ->
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
