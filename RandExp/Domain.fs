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

type SetItems =
    | SingleItem of char
    | Range of char * char

type Term =
    | CharLiteral of char
    | NegativeCharSet of SetItems array
    | RSet of SetItems array
    | SpecialChar of SpecialChar
    | Count of Term * Count
    | Group of Term array
    | Union of Term array * Term array

type RandomSchema = Term list
