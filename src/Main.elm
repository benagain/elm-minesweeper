module Main exposing (main)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import Html.Attributes exposing (style)
import MyCss exposing (..)
import List
import Random.Extra
import Random
import Html.Events exposing (onClick, defaultOptions)
import HtmlExtras exposing (onRightClick)


--import Tile exposing (initBomb, initClear)


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


type TileType
    = Bomb
    | Clear


type Tile
    = CoveredBomb
    | MarkedBomb
    | CoveredClear
    | MarkedClear
    | Exposed
    | Detonated



-- type alias Tile =
--     ( TileType, TileState )


type alias TileList =
    List Tile


type alias Model =
    { square : Int
    , tiles : TileList
    }



-- Init


type Msg
    = NewGame
    | NewBoard (List Tile)
    | DoClear Int
    | DoMark Int


init : ( Model, Cmd Msg )
init =
    ( Model 0 [], Random.generate NewBoard (randomBoard 5) )


randomBoard : Int -> Random.Generator (List Tile)
randomBoard size =
    randomListGenerator size


randomListGenerator : Int -> Random.Generator (List Tile)
randomListGenerator square =
    Random.list (square ^ 2) bombFlip



-- |> List.foldr (::) []
-- |> flatten


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



-- flatten : List (List TileType) -> List (TileType)
-- flatten list =
--     List.head list
--         |> Maybe.andThen Maybe.withDefault []
--         |> Maybe.withDefault Clear


toModel2 : List (Tile) -> Model
toModel2 list =
    Model ((List.length list) |> toFloat >> sqrt >> round) list



-- toModel : List (List Tile.Model) -> Dict Index Tile.Model
-- toModel tiles =
--     tiles
--         |> indexedTiles
--         |> List.foldr (++) []
--         |> Dict.fromList
--         |> countBombsForAllTiles
-- type alias XYTile =
--     ( Index, Tile.Model )
-- {-| Convert the 2D list of tiles into a 1D list of (x,y tile)
--     [ [ a, b, c ]
--     , [ d, e, f ]
--     ]
--     becomes
--     [ (0,0) a, (0, 1) b, (0, 2) c, (1, 0) d, (1, 1) e, (1, 2) f]
-- -}
-- indexedTiles : List (List Tile.Model) -> List (List XYTile)
-- indexedTiles list =
--     list
--         |> List.indexedMap
--             (\x row ->
--                 row
--                     |> List.indexedMap
--                         (\y tile -> ( ( x, y ), tile ))
--             )
-- countBombsForAllTiles : TileMap -> TileMap
-- countBombsForAllTiles tiles =
--     tiles |> Dict.map (countBombsForTile tiles)
-- countBombsForTile : TileMap -> Index -> Tile.Model -> Tile.Model
-- countBombsForTile tiles index tile =
--     tile |> Tile.addBombCount (numBombsAdjacent index tiles)
-- numBombsAdjacent : Index -> TileMap -> Int
-- numBombsAdjacent index tiles =
--     tiles
--         |> Dict.filter (onKey <| isAdjacent index)
--         |> Dict.filter (onValue Tile.isBomb)
--         |> Dict.size
-- isAdjacent : Index -> Index -> Bool
-- isAdjacent ( x, y ) ( x', y' ) =
--     (x == x' || x == x' + 1 || x == x' - 1)
--         && (y == y' || y == y' + 1 || y == y' - 1)
-- onKey : (comparable -> b) -> comparable -> a -> b
-- onKey f a _ =
--     (f a)


onValue : (a -> b) -> comparable -> a -> b
onValue f _ b =
    (f b)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Random.generate NewBoard (randomBoard 5) )

        NewBoard board ->
            ( toModel2 board, Cmd.none )

        DoClear xy ->
            ( expose xy model, Cmd.none )

        _ ->
            Debug.crash "TODO"


expose : Int -> Model -> Model
expose xy model =
    let
        d =
            Debug.log ("expose " ++ toString (xy) ++ " in ") (List.length model.tiles)
    in
        { model
            | tiles =
                replaceAt (exposeMe) xy model.tiles
        }


replaceAt : (a -> a) -> Int -> List a -> List a
replaceAt fn idx list =
    case ( list, idx ) of
        ( [], _ ) ->
            []

        ( head :: tail, 0 ) ->
            fn head :: tail

        ( head :: tail, i ) ->
            head :: replaceAt fn (i - 1) tail


exposeMe : Tile -> Tile
exposeMe tile =
    case (tile) of
        CoveredBomb ->
            Debug.log (toString (tile) ++ " -> ") Detonated

        CoveredClear ->
            Debug.log (toString (tile) ++ " -> ") Exposed

        other ->
            Debug.log (toString (tile) ++ " -> ") other


updateTiles : Index -> Model -> Model
updateTiles xy tiles =
    let
        index =
            0

        -- updated =
        --     Dict.update xy (Maybe.map (Tile.update msg)) tiles
        -- boom =
        --     Dict.get xy updated
        --         |> Maybe.map Tile.didDetonate
        --         |> maybeChoice identity (Dict.map (onValue Tile.expose))
    in
        tiles


maybeChoice : (a -> b) -> (a -> b) -> Maybe Bool -> a -> b
maybeChoice t f m =
    case m of
        Just True ->
            t

        _ ->
            f



-- View


view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        , style [ ( "width", toString (toFloat (model.square) * 2.4) ++ "em" ) ]
        ]
        (gameView model)


gameView : Model -> List (Html Msg)
gameView tiles =
    tiles.tiles |> List.indexedMap tileView



-- tileViewList : Index -> Tile.Model -> List (Html Msg) -> List (Html Msg)
-- tileViewList xy tile list =
--     (tileView xy tile) :: list


tileView : Int -> Tile -> Html Msg
tileView xy tile =
    Debug.log ("view " ++ toString (xy)) tview tile <| xy


tview : Tile -> Int -> Html Msg
tview tile =
    case tile of
        CoveredBomb ->
            viewWithNoText Nothing

        CoveredClear ->
            viewWithNoText Nothing

        MarkedBomb ->
            viewWithNoText (Just MyCss.MarkedTile)

        MarkedClear ->
            viewWithNoText (Just MyCss.MarkedTile)

        Exposed ->
            viewWithText (Just MyCss.ClearedTile) (toString "0")

        Detonated ->
            viewWithNoText (Just MyCss.DetonatedTile)


viewWithNoText : Maybe CssClasses -> Int -> Html Msg
viewWithNoText css xy =
    span ((cssFor css) ++ [ onClick (DoClear xy), onRightClick (DoMark xy) ]) []


viewWithText css text _ =
    span (cssFor css) [ Html.text (text) ]


cssFor css =
    mapWithDefault (\css -> [ class [ css ] ]) [] css


mapWithDefault : (a -> b) -> b -> Maybe a -> b
mapWithDefault fn b a =
    case a of
        Nothing ->
            b

        Just a ->
            fn a


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
