module Board exposing (generate, toModel, isBomb, happo, fourDirections)

import Set exposing (..)
import Random.Extra
import Random
import Model exposing (..)
import List.Extras exposing (takeIndices)


generate : Cmd Msg
generate =
    Random.generate NewBoard (randomBoard 5)


randomBoard : Int -> Random.Generator (List Tile)
randomBoard size =
    Random.list (size ^ 2) bombFlip


bombFlip : Random.Generator Tile
bombFlip =
    Random.map
        (\b ->
            if b then
                CoveredBomb
            else
                CoveredClear
        )
        (Random.Extra.oneIn 4)


toModel : List (Tile) -> Model
toModel list =
    let
        indexMap =
            List.indexedMap (,) list

        size =
            List.length list |> intSqrt
    in
        Model size (List.map (addBombs size list) indexMap)


intSqrt : Int -> Int
intSqrt int =
    int |> toFloat >> sqrt >> round


addBombs : Int -> List (Tile) -> ( Int, Tile ) -> ( Tile, Int )
addBombs size list ( idx, tile ) =
    ( tile, countBombsForTile size list idx )


countBombsForTile : Int -> List (Tile) -> Int -> Int
countBombsForTile size list index =
    takeIndices
        (fourDirections size index)
        list
        |> List.filter isBomb
        |> List.length


fourDirections : Int -> Int -> List Int
fourDirections size index =
    happo size index |> Set.toList


{-| Find the list of indices that comprise the square surrounding
    an index on a chess board.

    happo 5 8 = [ 2, 3, 4, 7, 9, 12, 13, 14 ]
-}
happo : Int -> Int -> Set Int
happo size index =
    let
        row =
            index // size

        indexOfRow =
            row * size
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
            |> Set.filter ((>=) ((size * size) - 1))


isBomb : Tile -> Bool
isBomb ground =
    ground == CoveredBomb || ground == MarkedBomb
