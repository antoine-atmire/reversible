module ReversibleTest exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Reversible exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Reversible module"
        [ describe "addition" testAddition
        , describe "multiplication" testMultiplication
        ]


testAddition : List Test
testAddition =
    [ test "add" <|
        \_ -> Expect.equal (apply (add 2) 1) (1 + 2)
    , test "reverse add" <|
        \_ ->
            Expect.equal (applyReverse (add 2) 1) (1 - 2)
    , test "subtract" <|
        \_ ->
            Expect.equal (apply (subtract 5) 25) (25 - 5)
    , test "reverse subtract" <|
        \_ ->
            Expect.equal (applyReverse (subtract 5) 25) (25 + 5)
    ]


testMultiplication : List Test
testMultiplication =
    let
        multiplyByTen =
            case multiplyByFloat 10 of
                Just reversibleFunction ->
                    reversibleFunction

                Nothing ->
                    Debug.todo "won't happen"

        divideByTen =
            case divideByFloat 10 of
                Just reversibleFunction ->
                    reversibleFunction

                Nothing ->
                    Debug.todo "won't happen"
    in
    [ test "multiply" <|
        \_ ->
            Expect.within (Absolute 0.000000001) (apply multiplyByTen 3) (3 * 10)
    , test "reverse multiply" <|
        \_ ->
            Expect.within (Absolute 0.000000001) (applyReverse multiplyByTen 3) (3 / 10)
    , test "divide" <|
        \_ ->
            Expect.within (Absolute 0.000000001) (apply divideByTen 3) (3 / 10)
    , test "reverse divide" <|
        \_ ->
            Expect.within (Absolute 0.000000001) (applyReverse divideByTen 3) (3 * 10)
    ]
