module Richiban.RandExp.Domain

type Count =
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

type Term =
    | CharLiteral of char
    | CharSet of char array
    | SpecialChar of SpecialChar
    | Count of Term * Count
    | Group of Term array

type RandomSchema = Term list
