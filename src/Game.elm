module Game exposing (..)

import Set exposing (..)


{-| Find the list of indices that comprise the square surrounding
    an index on a chess board.

    happho 5 8 = [ 2, 3, 4, 7, 9, 12, 13, 14 ]
-}
happho : Int -> Int -> Set Int
happho size index =
    let
        row =
            index // size

        indexOfRow =
            row * size

        -- indexOfPreviousRow =
        --     indexOfRow - size
        -- indexOfNextRow =
        --     indexOfRow + size
        -- indexInRow =
        --     index - indexOfRow
        d =
            Debug.log ("row " ++ toString (row) ++ ", indexOfRow " ++ toString (indexOfRow) ++ ", index ") index
    in
        Set.fromList
            [ -- nw
              (max (indexOfRow - size) (index - size - 1))
              -- n
            , index - size
              -- ne
            , (min (indexOfRow - 1) (index - size + 1))
              -- e
            , (max indexOfRow (index - 1))
              -- w
            , (min (indexOfRow + size - 1) (index + 1))
              -- sw
            , (max (indexOfRow + size) (index + size - 1))
              -- s
            , (index + size)
              -- se
            , (min (indexOfRow + size + size - 1) (index + size + 1))
            ]
            |> Set.remove index
            |> Set.filter ((<=) 0)
