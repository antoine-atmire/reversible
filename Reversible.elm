module Reversible exposing (..)


type ReversibleFunction a b
    = ReversibleFunction (a -> b) (b -> a)



-- modify reversible functions


reverse : ReversibleFunction a b -> ReversibleFunction b a
reverse (ReversibleFunction a b) =
    ReversibleFunction b a


pipe : ReversibleFunction a b -> ReversibleFunction b c -> ReversibleFunction a c
pipe (ReversibleFunction ab ba) (ReversibleFunction bc cb) =
    ReversibleFunction (ab >> bc) (cb >> ba)



-- put reversible functions to use


apply : ReversibleFunction a b -> (a -> b)
apply (ReversibleFunction function reverseFunction) =
    function


applyReverse : ReversibleFunction a b -> (b -> a)
applyReverse =
    reverse >> apply


map : ReversibleFunction a b -> (b -> b) -> a -> a
map (ReversibleFunction function reverse) transform =
    function >> transform >> reverse



-- reversible functions related to numbers


add : number -> ReversibleFunction number number
add constant =
    ReversibleFunction (\value -> value + constant) (\value -> value - constant)


subtract : number -> ReversibleFunction number number
subtract =
    add >> reverse
