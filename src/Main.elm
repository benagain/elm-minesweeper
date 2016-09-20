module Main exposing (main)

import Game exposing (..)
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
import Set


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
    | ExposedBomb
    | Detonated


type alias TileList =
    List ( Tile, Int )


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


toModel2 : List (Tile) -> Model
toModel2 list =
    let
        indexMap =
            List.indexedMap (,) list

        size =
            Debug.log "Model size" (List.length list |> intSqrt)
    in
        Model size (List.map (addBombs size list) indexMap)


addBombs : Int -> List (Tile) -> ( Int, Tile ) -> ( Tile, Int )
addBombs size list ( idx, tile ) =
    ( tile, countBombsForTile size list idx )


f : (a -> b) -> (b -> c) -> a -> b -> c
f fab fbc a b =
    fab a |> fbc


intSqrt : Int -> Int
intSqrt int =
    int |> toFloat >> sqrt >> round


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


isBomb : Tile -> Bool
isBomb ground =
    ground == CoveredBomb || ground == MarkedBomb


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
            Debug.log "expose " xy

        tile =
            List.drop xy model.tiles |> List.head

        detonated =
            Debug.log "detonated? "
                (tile
                    |> Maybe.map (fst >> isBomb)
                    |> Maybe.withDefault False
                )
    in
        case detonated of
            True ->
                { model
                    | tiles =
                        replaceAt (onFirst exposeMe) xy model.tiles
                            |> List.map (onFirst exposeTile)
                }

            False ->
                { model | tiles = replaceAt (onFirst exposeMe) xy model.tiles }


onFirst : (a -> a) -> ( a, b ) -> ( a, b )
onFirst fn a =
    ( fn <| fst a, snd a )


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
    Debug.log ("exposeMe " ++ toString (tile) ++ " -> ")
        (case tile of
            CoveredBomb ->
                Detonated

            other ->
                exposeTile other
        )


exposeTile : Tile -> Tile
exposeTile tile =
    Debug.log ("exposeTile " ++ toString (tile) ++ " -> ")
        (case (tile) of
            CoveredBomb ->
                ExposedBomb

            CoveredClear ->
                Exposed

            other ->
                other
        )


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
gameView model =
    model.tiles |> List.indexedMap (tileView model)



-- tileViewList : Index -> Tile.Model -> List (Html Msg) -> List (Html Msg)
-- tileViewList xy tile list =
--     (tileView xy tile) :: list


tileView : Model -> Int -> ( Tile, Int ) -> Html Msg
tileView model xy tile =
    tview model tile xy


tview : Model -> ( Tile, Int ) -> Int -> Html Msg
tview model ( tile, bombCount ) xy =
    case tile of
        CoveredBomb ->
            viewWithNoText2 Nothing xy "b"

        CoveredClear ->
            viewWithNoText Nothing xy

        MarkedBomb ->
            viewWithNoText (Just MyCss.MarkedTile) xy

        MarkedClear ->
            viewWithNoText (Just MyCss.MarkedTile) xy

        Exposed ->
            viewWithText (Just MyCss.ClearedTile) (toString <| bombCount)

        ExposedBomb ->
            viewWithNoText (Just MyCss.DetonatedTile) xy

        Detonated ->
            viewWithText (Just MyCss.DetonatedTile) "!"


viewWithNoText : Maybe CssClasses -> Int -> Html Msg
viewWithNoText css xy =
    span ((cssFor css) ++ [ onClick (DoClear xy), onRightClick (DoMark xy) ]) []


viewWithNoText2 : Maybe CssClasses -> Int -> String -> Html Msg
viewWithNoText2 css xy text =
    span ((cssFor css) ++ [ onClick (DoClear xy), onRightClick (DoMark xy) ]) [ Html.text (text) ]


viewWithText css text =
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
