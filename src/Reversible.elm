module Reversible exposing (ReversibleFunction(..), add, apply, applyReverse, characters, convertInteger, listMap, map, mapCharacters, pipe, reverse, subtract)

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


maybeMap : ReversibleFunction (Maybe a) (Maybe b) -> (b -> b) -> a -> Maybe a
maybeMap (ReversibleFunction function reverseFunction) transform =
    Just >> function >> Maybe.map transform >> reverseFunction



-- reversible functions related to strings


characters : ReversibleFunction String (List Char)
characters =
    ReversibleFunction String.toList String.fromList


mapCharacters : (Char -> Char) -> String -> String
mapCharacters =
    listMap characters


convertInteger : ReversibleFunction (Maybe String) (Maybe Int)
convertInteger =
    ReversibleFunction (Maybe.andThen String.toInt) (Maybe.map String.fromInt)



-- reversible functions related to math


add : number -> ReversibleFunction number number
add constant =
    ReversibleFunction (\value -> value + constant) (\value -> value - constant)


subtract : number -> ReversibleFunction number number
subtract =
    add >> reverse


multiplyByFloat : Float -> Maybe (ReversibleFunction Float Float)
multiplyByFloat constant =
    if constant == 0 then
        Nothing

    else
        Just <|
            ReversibleFunction
                (\value -> value * constant)
                (\value -> value / constant)


divideByFloat : Float -> Maybe (ReversibleFunction Float Float)
divideByFloat =
    multiplyByFloat >> Maybe.map reverse
