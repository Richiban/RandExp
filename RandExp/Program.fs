module Richiban.RandExp.Program

open System
open FParsec

open Richiban.RandExp.Parsing
open Richiban.RandExp.Output

[<EntryPoint>]
let main args =
    let input = args.[0]
    printfn "Running on input '%s'" input

    match parse input with
    | Result.Ok schema ->
        schema |> Seq.iter (printfn "%O")
        printfn "%s" (execute schema)
    | Result.Error err -> printfn "%O" err

    0
