module Reversible.Math exposing (add, subtract)

import Reversible exposing (ReversibleFunction(..), reverse)

-- reversible functions related to math


add : number -> ReversibleFunction number number
add constant =
    ReversibleFunction (\value -> value + constant) (\value -> value - constant)


subtract : number -> ReversibleFunction number number
subtract =
    add >> reverse
