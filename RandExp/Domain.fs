module Richiban.RandExp.Domain

type CountData =
    | ExactCount of int
    | MinCount of int
    | MaxCount of int
    | RangeCount of int * int

type SpecialChar =
    | AnyChar
    | AnyWordChar
    | AnyNonWordChar
    | AnyWhitespaceChar
    | AnyNonWhitespaceChar
    | AnyDigit
    | AnyNonDigit

type Mod = Count of CountData

type Term =
    | CharLiteral of char
    | CharSet of char array
    | SpecialChar of SpecialChar
    | Mod of Term * Mod

type RandomSchema = Term list
