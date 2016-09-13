module Main exposing (main)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List
import Dict exposing (Dict)
import Tile exposing (initBombTile, initClearTile)


main : Program Never
main =
    App.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Index =
    ( Int, Int )


type alias TileMap =
    Dict Index Tile.Model


type alias Model =
    TileMap


type Msg
    = SetTileState Index Tile.Msg



-- Init


model : Model
model =
    toModel tiles


toModel : List (List Tile.Model) -> Dict Index Tile.Model
toModel list =
    tiles
        |> indexedTiles
        |> List.foldr (++) []
        |> Dict.fromList
        |> countBombsForAllTiles


type alias XYTile =
    ( Index, Tile.Model )


{-| Convert the 2D list of tiles into a 1D list of (x,y tile)

    [ [ a, b, c ]
    , [ d, e, f ]
    ]
    becomes
    [ (0,0) a, (0, 1) b, (0, 2) c, (1, 0) d, (1, 1) e, (1, 2) f]
-}
indexedTiles : List (List Tile.Model) -> List (List XYTile)
indexedTiles list =
    list
        |> List.indexedMap
            (\x row ->
                row
                    |> List.indexedMap
                        (\y tile -> ( ( x, y ), tile ))
            )


countBombsForAllTiles : TileMap -> TileMap
countBombsForAllTiles tiles =
    tiles |> Dict.map (countBombsForTile tiles)


countBombsForTile : TileMap -> Index -> Tile.Model -> Tile.Model
countBombsForTile tiles index tile =
    tile |> Tile.addBombCount (numBombsAdjacent index tiles)


numBombsAdjacent : Index -> TileMap -> Int
numBombsAdjacent index tiles =
    tiles
        |> Dict.filter (onKey <| isAdjacent index)
        |> Dict.filter (onValue isBomb)
        |> Dict.size


isAdjacent : Index -> Index -> Bool
isAdjacent ( x, y ) ( x', y' ) =
    (x == x' || x == x' + 1 || x == x' - 1)
        && (y == y' || y == y' + 1 || y == y' - 1)


isBomb : Tile.Model -> Bool
isBomb { ground } =
    case ground of
        Tile.Bomb ->
            True

        Tile.Clear ->
            False


onKey : (Index -> Bool) -> Index -> Tile.Model -> Bool
onKey f a _ =
    (f a)


onValue : (Tile.Model -> Bool) -> Index -> Tile.Model -> Bool
onValue f _ b =
    (f b)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTileState xy msg ->
            updateTiles xy msg model


updateTiles : Index -> Tile.Msg -> TileMap -> TileMap
updateTiles xy msg tiles =
    let
        update =
            Dict.update xy (Maybe.map (Tile.update msg)) tiles

        boom =
            Dict.get xy update
    in
        case Maybe.map Tile.didDetonate boom of
            Just True ->
                Dict.map (\xy a -> Tile.expose a) update

            _ ->
                update



-- View


view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        ]
        (gameView model)


gameView : TileMap -> List (Html Msg)
gameView tiles =
    tiles |> Dict.foldr tileViewList []


tileViewList : Index -> Tile.Model -> List (Html Msg) -> List (Html Msg)
tileViewList xy tile list =
    (tileView xy tile) :: list


tileView : Index -> Tile.Model -> Html Msg
tileView xy tile =
    App.map (SetTileState xy) (Tile.view tile)


tiles : List (List Tile.Model)
tiles =
    [ [ initClearTile, initClearTile, initBombTile ]
    , [ initClearTile, initClearTile, initClearTile ]
    , [ initClearTile, initBombTile, initClearTile ]
    ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
