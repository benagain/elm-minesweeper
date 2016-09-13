module Main exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List
import Dict exposing (Dict)
import Tile exposing (bombTile, clearTile, clearTile')


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
            updateTiles xy msg model


updateTiles : Index -> Tile.Msg -> TileMap -> TileMap
updateTiles xy msg tiles =
    let
        update =
            Dict.update xy (Maybe.map (Tile.update msg)) tiles

        boom =
            Dict.get xy update
    in
        case boom of
            Just ( _, Tile.Detonated ) ->
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



-- indexedTile :
--     Int
--     -> Int
--     -> List (List Tile.Model)
--     -> ( Tile.Ground, Tile.TileState )
--     -> ( Index, ( Tile.Ground, Tile.TileState ) )


indexedTile x y tiles tile =
    case tile of
        ( Tile.Bomb, _ ) ->
            ( ( x, y ), tile )

        ( Tile.Clear n, _ ) ->
            ( ( x, y ), clearTile' (numBombsAdjacent ( x, y ) tiles) )


numBombsAdjacent : Index -> TileMap -> Int
numBombsAdjacent index tiles =
    tiles
        |> (filt index)
        |> Dict.size


filt : Index -> TileMap -> TileMap
filt index map =
    map
        |> Dict.filter (dropValue (isAdjacent index))
        |> Dict.filter (dropKey isBomb)


isAdjacent : Index -> Index -> Bool
isAdjacent ( x, y ) ( x', y' ) =
    (x == x' || x == x' + 1 || x == x' - 1)
        && (y == y' || y == y' + 1 || y == y' - 1)


isBomb : Tile.Model -> Bool
isBomb ( tileType, _ ) =
    case tileType of
        Tile.Bomb ->
            True

        Tile.Clear _ ->
            False


dropValue : (Index -> Bool) -> Index -> Tile.Model -> Bool
dropValue f a _ =
    (f a)


dropKey : (Tile.Model -> Bool) -> Index -> Tile.Model -> Bool
dropKey f _ b =
    (f b)


hashTiles : List (List Tile.Model) -> Dict Index Tile.Model
hashTiles list =
    Dict.fromList (List.foldr (++) [] (indexedTiles tiles))
        |> addKnowledge


addKnowledge : TileMap -> TileMap
addKnowledge tiles =
    tiles |> Dict.map (know tiles)


know : TileMap -> Index -> Tile.Model -> Tile.Model
know tiles index ( tile, state ) =
    case tile of
        Tile.Bomb ->
            ( tile, state )

        Tile.Clear _ ->
            clearTile' (numBombsAdjacent index tiles)



-- know : Dict ( Index, Tile.Model ) -> Index -> Tile.Model -> Tile.Model
-- know tiles index ( tile, state ) =
--     case tile of
--         Tile.Bomb ->
--             ( tile, state )
-- Tile.Clear _ ->
--     clearTile' (numBombsAdjacent index tiles)


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
