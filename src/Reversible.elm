module Reversible exposing (ReversibleFunction(..), apply, applyReverse, characters, listMap, map, mapCharacters, pipe, reverse)

-- create and modify reversible functions


type ReversibleFunction a b
    = ReversibleFunction (a -> b) (b -> a)


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
map (ReversibleFunction function reverseFunction) transform =
    function >> transform >> reverseFunction


listMap : ReversibleFunction a (List b) -> (b -> b) -> a -> a
listMap (ReversibleFunction function reverseFunction) transform =
    function >> List.map transform >> reverseFunction



-- reversible functions related to strings


characters : ReversibleFunction String (List Char)
characters =
    ReversibleFunction String.toList String.fromList


mapCharacters : (Char -> Char) -> String -> String
mapCharacters =
    listMap characters



