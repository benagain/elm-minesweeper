module Main exposing (main)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import Html.Attributes exposing (style)
import MyCss exposing (..)
import List
import Dict exposing (Dict)
import Tile exposing (initBomb, initClear)
import Random.Extra
import Random


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Index =
    ( Int, Int )


type alias TileMap =
    Dict Index Tile.Model


type alias Model =
    TileMap



-- Init


init : ( Model, Cmd Msg )
init =
    ( Dict.empty, Random.generate NewBoard randomBoard )



-- model : Model
-- model =


toModel : List (List Tile.Model) -> Dict Index Tile.Model
toModel tiles =
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
        |> Dict.filter (onValue Tile.isBomb)
        |> Dict.size


isAdjacent : Index -> Index -> Bool
isAdjacent ( x, y ) ( x', y' ) =
    (x == x' || x == x' + 1 || x == x' - 1)
        && (y == y' || y == y' + 1 || y == y' - 1)


onKey : (comparable -> b) -> comparable -> a -> b
onKey f a _ =
    (f a)


onValue : (a -> b) -> comparable -> a -> b
onValue f _ b =
    (f b)



-- UPDATE


type Msg
    = NewGame
    | NewBoard (List (List Tile.Model))
    | SetTileState Index Tile.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Random.generate NewBoard randomBoard )

        NewBoard board ->
            ( toModel board, Cmd.none )

        SetTileState xy msg ->
            ( updateTiles xy msg model, Cmd.none )


randomBoard : Random.Generator (List (List Tile.Model))
randomBoard =
    randomListGenerator 5


updateTiles : Index -> Tile.Msg -> TileMap -> TileMap
updateTiles xy msg tiles =
    let
        updated =
            Dict.update xy (Maybe.map (Tile.update msg)) tiles

        boom =
            Dict.get xy updated
    in
        case Maybe.map Tile.didDetonate boom of
            Just True ->
                updated |> Dict.map (onValue Tile.expose)

            _ ->
                updated



-- View


view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        , style [ ( "width", toString ((sqrt (toFloat (Dict.size model))) * 2.4) ++ "em" ) ]
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
    [ [ initClear, initClear, initBomb ]
    , [ initClear, initClear, initClear ]
    , [ initClear, initBomb, initClear ]
    ]


randomTiles : List (List Tile.Model)
randomTiles =
    let
        seed =
            Random.initialSeed (12345)

        ( t, s ) =
            Random.step (randomListGenerator 5) seed
    in
        t


randomListGenerator : Int -> Random.Generator (List (List Tile.Model))
randomListGenerator n =
    Random.list n (Random.list n bombFlip)



-- let
--     ( val, nexGen ) =
--         Random.bool ()
--             Random.initialSeed
--             31415
-- in
--     Random.step bombFlip seed0
-- Random.generate (Random.list n bombFlip)


bombFlip : Random.Generator Tile.Model
bombFlip =
    Random.map
        (\b ->
            if b then
                initBomb
            else
                initClear
        )
        (Random.Extra.oneIn 10)



-- Random.map
--     (\b ->
--         if b then
--             initClear
--         else
--             initBomb
--     )
--     Random.bool


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
