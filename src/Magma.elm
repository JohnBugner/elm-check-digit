module Magma exposing (..)

import Dict
import Dict.Extra
import List.Extra

type alias Magma a =
    { alphabet : List a
    , startChar : a
    , d : Dict.Dict (a,a) a
    }

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
                Just y -> Dict.get (x,y) magma.d
                Nothing -> Nothing
    in
        List.foldl f (Just magma.startChar) chars

allCheckCharPairsAreDifferent : Magma comparable -> List (List comparable, List comparable) -> Bool
allCheckCharPairsAreDifferent magma pairs = List.all (\ (as_,bs) -> checkChar magma as_ /= checkChar magma bs) pairs
--allCheckCharPairsAreDifferent magma pairs =
--    let
--        f (as_,bs) =
--            let
--                r = checkChar magma as_ /= checkChar magma bs
--            in
--                if r
--                then r
--                else Debug.log (String.join " " [Debug.toString as_, Debug.toString bs, Debug.toString <| checkChar magma as_]) r
--    in
--        List.all identity <| List.map f pairs

-- Is 'i' the identity element ?
-- (x + i == x) && (i + y == y)
hasIdentityElementOf : comparable -> Magma comparable -> Bool
hasIdentityElementOf i magma =
    ((List.filterMap (\ x -> Dict.get (x,i) magma.d) magma.alphabet) == magma.alphabet) &&
    ((List.filterMap (\ y -> Dict.get (i,y) magma.d) magma.alphabet) == magma.alphabet)

-- Sets the identity element to 'i'.
normalize : comparable -> Magma comparable -> Magma comparable
normalize i magma =
    let
        normalizeRow : Dict.Dict (comparable,comparable) comparable -> Dict.Dict (comparable,comparable) comparable
        normalizeRow d =
            let
                d_ = Dict.Extra.filterMap (\ _ v -> Dict.get v valueConversions) d
                valueConversions = Dict.fromList <| List.Extra.zip identityRow magma.alphabet
                identityRow = List.filterMap (\ x -> Dict.get (x,i) magma.d) magma.alphabet
            in
                d_

        normalizeColumn : Dict.Dict (comparable,comparable) comparable -> Dict.Dict (comparable,comparable) comparable
        normalizeColumn d =
            let
                d_ = Dict.fromList <| List.filterMap f <| Dict.toList d
                f ((x,y),v) =
                    case Dict.get y keyYConversions of
                        Just y_ -> Just ((x,y_),v)
                        Nothing -> Nothing
                keyYConversions = Dict.fromList <| List.Extra.zip magma.alphabet identityColumn
                identityColumn = List.filterMap (\ y -> Dict.get (i,y) magma.d) magma.alphabet
            in
                d_
    in
        Magma magma.alphabet magma.startChar (normalizeColumn <| normalizeRow magma.d)

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
