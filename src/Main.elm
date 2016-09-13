module Main exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List
import Dict exposing (Dict)
import Tile exposing (bombTile, clearTile)


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


model : Model
model =
    hashTiles tiles



-- UPDATE


type Msg
    = SetTileState Index Tile.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTileState xy msg ->
            updateTiles xy model


updateTiles : Index -> TileMap -> TileMap
updateTiles xy tiles =
    Dict.update xy (Maybe.map Tile.showTile) tiles



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


type alias XYTile =
    ( Index, Tile.Model )


indexedTiles : List (List Tile.Model) -> List (List XYTile)
indexedTiles list =
    list
        |> List.indexedMap
            (\x row ->
                row
                    |> List.indexedMap
                        (\y tile -> ( ( x, y ), tile ))
            )


hashTiles : List (List Tile.Model) -> Dict Index Tile.Model
hashTiles list =
    Dict.fromList (List.foldr (++) [] (indexedTiles tiles))


tiles : List (List Tile.Model)
tiles =
    [ [ clearTile, clearTile, bombTile ]
    , [ clearTile, clearTile, clearTile ]
    , [ clearTile, bombTile, clearTile ]
    ]


opptiles : List (List Tile.Model)
opptiles =
    Tile.showAll tiles



-- tiles =
--     [ [ notABomb, ( Tile.Clear, Tile.Cleared 1 ), ( Tile.Bomb, Tile.Marked ) ]
--     , [ ( Tile.Clear, Tile.Cleared 2 ), ( Tile.Clear, Tile.Cleared 3 ), notABomb ]
--     , [ notABomb, ( Tile.Bomb, Tile.Marked ), notABomb ]
--     ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
