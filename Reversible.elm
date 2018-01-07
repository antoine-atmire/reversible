module Reversible exposing (..)


type ReversibleFunction ab ba
    = ReversibleFunction (ab -> ba) (ba -> ab)



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


listMap : ReversibleFunction a (List b) -> (b -> b) -> a -> a
listMap (ReversibleFunction function reverse) transform =
    function >> List.map transform >> reverse



-- reversible functions related to strings


characters : ReversibleFunction String (List Char)
characters =
    ReversibleFunction String.toList String.fromList


mapCharacters : (Char -> Char) -> String -> String
mapCharacters = 
        listMap characters


-- reversible functions related to numbers


add : number -> ReversibleFunction number number
add constant =
    ReversibleFunction (\value -> value + constant) (\value -> value - constant)


subtract : number -> ReversibleFunction number number
subtract =
    add >> reverse
