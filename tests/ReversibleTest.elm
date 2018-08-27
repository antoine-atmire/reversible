module ReversibleTest exposing (suite)

import Expect
import Reversible exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Reversible module"
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
