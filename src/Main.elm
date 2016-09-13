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
                    indexMap x (updateRow y) model
            in
                updated



-- ++ updated


updateRow : Int -> List XYTile -> List XYTile
updateRow index list =
    list |> indexMap index (updateWhen 0 0)



-- list |> List.map (List.indexedMap (updateWhen index))


updateWhen : Int -> Int -> XYTile -> XYTile
updateWhen when index ( x, y, tile ) =
    case when == index of
        True ->
            ( x, y, (Tile.showTile tile) )

        False ->
            ( x, y, tile )


indexMap : Int -> (a -> a) -> List a -> List a
indexMap index fn list =
    iindexMap index fn list


iindexMap : Int -> (a -> a) -> List a -> List a
iindexMap index fn list =
    case ( index, list ) of
        ( _, [] ) ->
            []

        ( 0, h :: t ) ->
            (fn h) :: t

        ( _, h :: t ) ->
            h :: iindexMap (index - 1) fn t



-- Tile.DoClear ->
--     model
-- Tile.DoMark ->
--     model
-- VIEW


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        ]
        (gameView model)



-- (gameView tiles)
-- gameView : List (List Tile.Model) -> List (Html msg)


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
    List.indexedMap
        (\x a ->
            List.indexedMap
                (\y t -> ( x, y, t ))
                a
        )
        list


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
