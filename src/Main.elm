module Main exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List
import Tile exposing (bombTile, clearTile)


-- import Css exposing (..)
-- import Html.Events exposing (onClick)


main : Program Never
main =
    App.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    TileMap


model : Model
model =
    indexedTiles tiles



-- UPDATE


type Msg
    = SetTileState ( Int, Int ) Tile.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTileState ( x, y ) m ->
            let
                updated =
                    updateMap ( x, y ) model
            in
                updated


updateMap : ( Int, Int ) -> TileMap -> TileMap
updateMap xy map =
    map |> List.map (updateRow xy)


updateRow : ( Int, Int ) -> List (XYTile) -> List (XYTile)
updateRow xy row =
    row |> List.map (updateTile xy)


updateTile : ( Int, Int ) -> XYTile -> XYTile
updateTile xy' ( x, y, tile ) =
    let
        updated =
            if xy' == ( x, y ) then
                Tile.showTile tile
            else
                tile
    in
        ( x, y, updated )


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        ]
        (gameView model)


gameView : TileMap -> List (Html Msg)
gameView tiles =
    tiles
        |> List.foldr (++) []
        |> List.map myTileView


myTileView : XYTile -> Html Msg
myTileView ( x, y, tile ) =
    App.map (SetTileState ( x, y )) (Tile.view tile)


type alias XYTile =
    ( Int, Int, Tile.Model )


type alias TileMap =
    List (List XYTile)


indexedTiles : List (List Tile.Model) -> List (List XYTile)
indexedTiles list =
    list
        |> List.indexedMap
            (\x row ->
                row
                    |> List.indexedMap
                        (\y tile -> ( x, y, tile ))
            )


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
