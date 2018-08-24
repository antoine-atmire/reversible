module Reversible.MathTest exposing (suite)

import Expect
import Reversible exposing (..)
import Reversible.Math as Math
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Reversible.Math module"
        [ test "add" <|
            \_ -> Expect.equal (apply (Math.add 2) 1) (1 + 2)
        , test "reverse add" <|
            \_ ->
                Expect.equal (applyReverse (Math.add 2) 1) (1 - 2)
        , test "subtract" <|
            \_ ->
                Expect.equal (apply (Math.subtract 5) 25) (25 - 5)
        , test "reverse subtract" <|
            \_ ->
                Expect.equal (applyReverse (Math.subtract 5) 25) (25 + 5)
        ]
