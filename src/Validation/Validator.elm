module Validation.Validator exposing (..)

import List
import Regex
import Validation exposing (Validation, invalid, valid)


notBlank : e -> String -> Validation e String
notBlank err s =
    if String.isEmpty s then
        invalid err
    else
        valid s


minLength : Int -> (Int -> e) -> String -> Validation e String
minLength n mkErr s =
    if String.length s < n then
        invalid (mkErr n)
    else
        valid s


maxLength : Int -> (Int -> e) -> String -> Validation e String
maxLength n mkErr s =
    if String.length s > n then
        invalid (mkErr n)
    else
        valid s


isNumber : e -> String -> Validation e String
isNumber err s =
    case String.toInt s of
        Err _ ->
            invalid err

        Ok _ ->
            valid s


isEmail : e -> String -> Validation e String
isEmail err s =
    let
        emailRE =
            Regex.regex "^.+@.+$" |> Regex.caseInsensitive
    in
    if Regex.contains emailRE s then
        valid s
    else
        invalid err


notInList : e -> List a -> a -> Validation e a
notInList err list value =
    if List.member value list then
        invalid err
    else
        valid value


inList : e -> List a -> a -> Validation e a
inList err list value =
    if List.member value list then
        valid value
    else
        invalid err
