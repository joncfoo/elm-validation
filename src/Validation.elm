module Validation
    exposing
        ( Validation
        , also
        , andThen
        , apply
        , bimap
        , invalid
        , map
        , transform
        , valid
        )

{-| Validation

@docs Validation


# Constructing a validation type

@docs valid, invalid


# Modifying a validation

@docs transform, map, bimap


# Applying successive validations

@docs apply, also


# Chaining validations

@docs andThen

-}

import Cons exposing (Cons)


type alias NonEmptyList e =
    Cons e


{-| Validation type
-}
type Validation e a
    = Invalid (NonEmptyList e)
    | Valid a


{-| Fail with a validation error.

    isEmpty : String -> Validation Error String
    isEmpty str =
        case String.isEmpty str of
            True ->
                valid str

            False ->
                invalid IsEmpty

-}
invalid : e -> Validation e a
invalid e =
    Invalid (Cons.singleton e)


{-| Succeed with a validation result.

    isEmpty : String -> Validation Error String
    isEmpty str =
        case String.isEmpty str of
            True ->
                valid str

            False ->
                invalid IsEmpty

-}
valid : a -> Validation e a
valid a =
    Valid a


{-| Transforms the validation into another type.

    displayValidation : Validation Error User -> Html msg
    displayValidation v =
        case transform Err Ok v of
            Err errors ->
                div []
                    Cons.map
                    toString
                    errors
                    |> Cons.toList

            Ok user ->
                div [] [ toString user ]

-}
transform : (Cons e -> b) -> (a -> b) -> Validation e a -> b
transform fnE fnA v =
    case v of
        Invalid e ->
            fnE e

        Valid a ->
            fnA a


{-| Applies a function over the successful part of the validation.

    type Email
        = Email String

    toEmail : Validation e String -> Validation e Email
    toEmail v =
        map Email v

-}
map : (a -> b) -> Validation e a -> Validation e b
map fn v =
    case v of
        Invalid e ->
            Invalid e

        Valid a ->
            Valid (fn a)


{-| Run functions over the failure or success part of the validation.
-}
bimap : (e -> f) -> (a -> b) -> Validation e a -> Validation f b
bimap fnE fnA v =
    case v of
        Invalid e ->
            Invalid (Cons.map fnE e)

        Valid a ->
            Valid (fnA a)


{-| Applies a function contained in the success part of the validation to another validation.
This can be used like so:

    validUsername : String -> Validation Error Username

    validEmail : String -> Validation Error Email

    validPassword : String -> Validation Error Password

    type alias User =
        { username : Username
        , email : Email
        , password : Password
        }

    validUser : Validation Error User
    validUser =
        User
            |> apply (validUsername formValues.username
            |> apply (validEmail formValues.email
            |> apply (validPassword formValues.password)

-}
apply : Validation e a -> Validation e (a -> b) -> Validation e b
apply v vFn =
    case vFn of
        Invalid e1 ->
            case v of
                Invalid e2 ->
                    Invalid (Cons.append e1 e2)

                Valid a2 ->
                    Invalid e1

        Valid fn ->
            map fn v


{-| Combines two validations but keeps the success part of the first one.
The effect of this is that the errors will be accumulated.
-}
also : Validation e b -> Validation e a -> Validation e a
also v2 v1 =
    apply v2 (map always v1)


{-| Chain a validation.
-}
andThen : (a -> Validation e b) -> Validation e a -> Validation e b
andThen fn v =
    case v of
        Invalid e ->
            Invalid e

        Valid a ->
            fn a
