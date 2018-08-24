module ReversibleTest exposing (suite)

import Expect
import Reversible
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Reversible module"
        [ test "setup" <|
            \_ -> Expect.equal 1 1
        ]
