module Model exposing (..)

import Game exposing (..)
import Set
import Random.Extra
import Random


type Tile
    = CoveredBomb
    | CoveredClear
    | MarkedBomb
    | MarkedClear
    | ExposedClear
      -- only shown when the game is lost
    | ExposedBomb
    | Detonated


type alias TileList =
    List ( Tile, Int )


type alias Model =
    { square : Int
    , tiles : TileList
    }


type Msg
    = NewGame
    | NewBoard (List Tile)
    | DoClear Int
    | DoMark Int


generateCommand =
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
            Debug.log "Model size" (List.length list |> intSqrt)
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
    Debug.log
        ("Count bombs for " ++ toString (index))
        (takeIndices
            (fourDirections size index)
            list
            |> List.filter isBomb
            |> List.length
        )


fourDirections : Int -> Int -> List Int
fourDirections size index =
    Game.happho size index |> Set.toList


takeIndices : List Int -> List a -> List a
takeIndices indices xs =
    Debug.log ("indices " ++ toString (indices)) (takeIndices_ 0 indices xs)


takeIndices_ : Int -> List Int -> List a -> List a
takeIndices_ idx indices xs =
    let
        thisOne =
            List.filter ((==) idx) indices
    in
        case ( thisOne, xs ) of
            ( _, [] ) ->
                []

            ( [], head :: tail ) ->
                takeIndices_ (idx + 1) indices tail

            ( ihead :: _, head :: tail ) ->
                head :: takeIndices_ (idx + 1) indices tail


isBomb : Tile -> Bool
isBomb ground =
    ground == CoveredBomb || ground == MarkedBomb
