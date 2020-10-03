module Richiban.RandExp.Program

open System
open FParsec

open Richiban.RandExp.Parsing1

let input = "cd{2,0}"//"([Ww]ub){2,5}"

match parse input with
| Result.Ok result -> result |> Seq.iter (printfn "%O")
| Result.Error err -> printfn "%O" err
