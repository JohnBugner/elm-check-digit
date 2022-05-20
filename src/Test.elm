module Test exposing (..)

import Magma

import List

-- Taken from wikiversity ( https://en.wikiversity.org/wiki/Damm_algorithm ),
-- which was taken from Michael Damm's doctoral dissertation
-- ( https://archiv.ub.uni-marburg.de/diss/z2004/0516/pdf/dhmd.pdf ),
-- page 111, and then modified so the diagonal is all '0's.
damm : Magma.Magma Int
damm =
    let
        ll =
            [ [0,3,1,7,5,9,8,6,4,2]
            , [7,0,9,2,1,5,4,8,6,3]
            , [4,2,0,6,8,7,1,3,5,9]
            , [1,7,5,0,9,8,3,4,2,6]
            , [6,1,2,3,0,4,5,9,7,8]
            , [3,6,7,4,2,0,9,5,8,1]
            , [5,8,6,9,7,2,0,1,3,4]
            , [8,9,4,5,3,6,2,0,1,7]
            , [9,4,3,8,6,1,7,2,0,5]
            , [2,5,8,1,4,3,6,7,9,0]
            ]
    in
        Magma.Magma (List.range 0 9) 0 (Magma.fromListToDict ll)
