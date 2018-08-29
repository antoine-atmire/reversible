module ReversibleTest exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Reversible exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Reversible module"
        [ describe "addition" testAddition
        , describe "multiplication" testMultiplication
        , describe "strings" testStrings
        , describe "maps" testMaps
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
            equalFloats (apply multiplyByTen 3) (3 * 10)
    , test "reverse multiply" <|
        \_ ->
            equalFloats (applyReverse multiplyByTen 3) (3 / 10)
    , test "divide" <|
        \_ ->
            equalFloats (apply divideByTen 3) (3 / 10)
    , test "reverse divide" <|
        \_ ->
            equalFloats (applyReverse divideByTen 3) (3 * 10)
    ]


equalFloats : Float -> Float -> Expectation
equalFloats =
    Expect.within (Absolute 0.000000001)


testStrings : List Test
testStrings =
    [ test "characters" <|
        \_ ->
            Expect.equal (apply characters "abc") [ 'a', 'b', 'c' ]
    , test "reverse characters" <|
        \_ ->
            Expect.equal (applyReverse characters [ 'a', 'b', 'c' ]) "abc"
    , test "convertInteger" <|
        \_ ->
            Expect.equal (apply convertInteger (Just "91")) (Just 91)
    , test "convertInteger not an integer" <|
        \_ ->
            Expect.equal (apply convertInteger (Just "a")) Nothing
    , test "reverse convertInteger" <|
        \_ ->
            Expect.equal (applyReverse convertInteger (Just 91)) (Just "91")
    , test "reverse convertInteger nothing" <|
        \_ ->
            Expect.equal (applyReverse convertInteger Nothing) Nothing
    ]


testMaps : List Test
testMaps =
    [ test "map" <|
        \_ ->
            Expect.equal
                (map switchCoordinateStructure (\{ x, y } -> { x = y, y = x }) ( 0, 5 ))
                ( 5, 0 )
    , test "listMap" <|
        \_ ->
            Expect.equal (listMap characters Char.toUpper "a/b/c") "A/B/C"
    , test "maybeMap" <|
        \_ ->
            Expect.equal (maybeMap convertInteger ((*) 2) "111") (Just "222")
    ]
