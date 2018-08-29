module Reversible exposing (ReversibleFunction(..), add, apply, applyReverse, charCode, characters, combine, convertInteger, divideByFloat, liftList, liftMaybe, listMap, map, maybeMap, multiplyByFloat, pipe, reverse, subtract, switchCoordinateStructure)

-- create and modify reversible functions


type ReversibleFunction a b
    = ReversibleFunction (a -> b) (b -> a)


reverse : ReversibleFunction a b -> ReversibleFunction b a
reverse (ReversibleFunction a b) =
    ReversibleFunction b a


combine : ReversibleFunction a b -> ReversibleFunction b c -> ReversibleFunction a c
combine (ReversibleFunction ab ba) (ReversibleFunction bc cb) =
    ReversibleFunction (ab >> bc) (cb >> ba)


pipe : ReversibleFunction b c -> ReversibleFunction a b -> ReversibleFunction a c
pipe second first =
    combine first second



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


liftList : ReversibleFunction a b -> ReversibleFunction (List a) (List b)
liftList (ReversibleFunction function reverseFunction) =
    ReversibleFunction (List.map function) (List.map reverseFunction)


liftMaybe : ReversibleFunction a b -> ReversibleFunction (Maybe a) (Maybe b)
liftMaybe (ReversibleFunction function reverseFunction) =
    ReversibleFunction (Maybe.map function) (Maybe.map reverseFunction)



-- reversible functions related to strings


characters : ReversibleFunction String (List Char)
characters =
    ReversibleFunction String.toList String.fromList


charCode : ReversibleFunction Char Int
charCode =
    ReversibleFunction Char.toCode Char.fromCode


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


switchCoordinateStructure : ReversibleFunction ( number, number ) { x : number, y : number }
switchCoordinateStructure =
    ReversibleFunction (\( x, y ) -> { x = x, y = y }) (\{ x, y } -> ( x, y ))
