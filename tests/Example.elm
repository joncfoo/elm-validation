module Example exposing (..)

import Expect exposing (Expectation, equal)
import Test exposing (..)
import Validation exposing (..)


suite : Test
suite =
    let
        fnE =
            \x -> x ++ "!!"

        fnA =
            \y -> y + 32
    in
    describe "Validation"
        [ describe "map"
            [ test "should apply the function over the success part" <|
                \_ ->
                    equal (map fnA (valid 0)) (valid 32)
            ]
        , describe "bimap"
            [ test "should map over the error" <|
                \_ ->
                    equal (bimap fnE fnA (invalid "oops")) (invalid "oops!!")
            , test "should map over the success" <|
                \_ ->
                    equal (bimap fnE fnA (valid 0)) (valid 32)
            ]
        , describe "apply"
            [ test "should apply the function in the success part" <|
                \_ ->
                    equal (valid fnA <*> valid 0) (valid 32)
            ]
        , describe "applyFirst"
            [ test "should keep the result of the initial validation" <|
                \_ ->
                    equal (valid 0 <* valid 100) (valid 0)
            ]
        , describe "applySecond"
            [ test "should keep the result of the latter validation" <|
                \_ ->
                    equal (valid 0 *> valid 100) (valid 100)
            ]
        , describe "unV"
            [ test "should convert to another data-type" <|
                \_ ->
                    equal (transform Err Ok <| valid "foo@bar.com") (Ok "foo@bar.com")
            ]
        ]
