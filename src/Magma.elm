module Magma exposing (..)

import Dict
import Dict.Extra
import List.Extra

type alias Magma a =
    { alphabet : List a
    , startChar : a
    , dict : Dict.Dict (a,a) a
    }

range2d : (Int, Int) -> List (Int,Int)
range2d (w,h) =
    let
        f xys =
            case xys of
                y :: x :: _ -> Just (x,y)
                _ -> Nothing
    in
        List.filterMap f <|
        List.Extra.cartesianProduct [List.range 0 w, List.range 0 h]

toString : Magma Int -> String
toString magma =
    let
        xys = range2d (9,9)
    in
        String.join "\n" <|
        List.map (String.join " ") <|
        List.Extra.greedyGroupsOf (List.length magma.alphabet) <|
        List.map String.fromInt <|
        List.filterMap identity <|
        List.map (\ xy -> Dict.get xy magma.dict) xys

fromListToDict : List (List a) -> Dict.Dict (Int,Int) a
fromListToDict ll =
    Dict.fromList <|
    List.concat <|
    (List.indexedMap (\ y l -> List.indexedMap (\ x v -> ((x,y),v)) l) ll)

checkChar : Magma comparable -> List comparable -> Maybe comparable
checkChar magma chars =
    let
        f x my =
            case my of
                Just y -> Dict.get (x,y) magma.dict
                Nothing -> Nothing
    in
        List.foldl f (Just magma.startChar) chars

allCheckCharPairsAreDifferent : Magma comparable -> List (List comparable, List comparable) -> Bool
allCheckCharPairsAreDifferent magma pairs = List.all (\ (as_,bs) -> checkChar magma as_ /= checkChar magma bs) pairs

-- Is 'i' the left identity element ?
-- (i + y == y)
isLeftIdentityElement : comparable -> Magma comparable -> Bool
isLeftIdentityElement i magma =
    ((List.filterMap (\ x -> Dict.get (x,i) magma.dict) magma.alphabet) == magma.alphabet)

-- Sets the left identity element to 'i'.
normalize : comparable -> Magma comparable -> Magma comparable
normalize i magma =
    let
        normalizeRow : Dict.Dict (comparable,comparable) comparable -> Dict.Dict (comparable,comparable) comparable
        normalizeRow dict =
            let
                newDict = Dict.Extra.filterMap (\ _ v -> Dict.get v valueConversions) dict
                valueConversions = Dict.fromList <| List.Extra.zip values newValues
                values = List.filterMap (\ x -> Dict.get (x,i) magma.dict) magma.alphabet
                newValues = magma.alphabet
            in
                newDict
    in
        Magma magma.alphabet magma.startChar (normalizeRow magma.dict)

-- Can it detect every 'a' -> 'b' error ?
-- (if c + a == c + b then a == b) &&
-- (if a + c == b + c then a == b)
isQuasiGroup : Magma comparable -> Bool
isQuasiGroup magma =
    let
        pairs = List.map (\ (a,b) -> ([a],[b])) <| List.Extra.uniquePairs magma.alphabet
        columnPairs = List.concatMap (\ c -> List.map (\ (a,b) -> (c::a,c::b)) pairs) magma.alphabet
        rowPairs = List.concatMap (\ c -> List.map (\ (a,b) -> (a++[c],b++[c])) pairs) magma.alphabet
    in
        allCheckCharPairsAreDifferent magma (columnPairs ++ rowPairs)

-- Can it detect every 'ab' -> 'ba' error ?
-- (if a + b == b + a then a == b) &&
-- (if c + a + b == c + b + a then a == b)
isTotallyAntiSymmetric : Magma comparable -> Bool
isTotallyAntiSymmetric magma =
    let
        pairs = List.map (\ (a,b) -> ([a,b],[b,a])) <| List.Extra.uniquePairs magma.alphabet
        triples = List.concatMap (\ c -> List.map (\ (a,b) -> (c::a,c::b)) pairs) magma.alphabet
    in
        allCheckCharPairsAreDifferent magma (pairs ++ triples)

-- Can it detect errors where "-teen" (like in 13) is misheard as "-ty" (like in 30) ?
-- Can it detect every '1v' -> 'a0' error ?, where 'a' is between 3 and 9 inclusive.
-- (if 1 + a == a + 0 then False) &&
-- (if c + 1 + a == c + a + 0 then False)
germanicPairsAreTotallyAntiSymmetric : Magma Int -> Bool
germanicPairsAreTotallyAntiSymmetric magma =
    let
        pairs = List.map (\ a -> ([1,a],[a,0])) <| List.range 3 9
        triples = List.concatMap (\ c -> List.map (\ (a,b) -> (c::a,c::b)) pairs) magma.alphabet
    in
        allCheckCharPairsAreDifferent magma (pairs ++ triples)
