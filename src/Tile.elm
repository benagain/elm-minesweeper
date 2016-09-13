module Tile
    exposing
        ( Model
        , Msg(..)
        , TileState(..)
        , Ground(..)
        , view
        , bombTile
        , clearTile
        , clearTile'
        , update
        , showAll
        , markTile
        , expose
        )

import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick, defaultOptions)
import Html.CssHelpers
import Json.Decode as Json exposing ((:=))
import MyCss


type Ground
    = Bomb
    | Clear


type TileState
    = Blank
    | Marked
    | Cleared
    | Detonated


type alias Model =
    { ground : Ground, state : TileState, adjacentBombs : Int }


type Msg
    = DoClear
    | DoMark


bombTile =
    { clearTile | ground = Bomb }


clearTile =
    { ground = Clear, state = Blank, adjacentBombs = 0 }


clearTile' numBombs =
    { clearTile | adjacentBombs = numBombs }


update : Msg -> Model -> Model
update msg model =
    case msg of
        DoClear ->
            sweepTile model

        DoMark ->
            markTile model


view : Model -> Html Msg
view { ground, state, adjacentBombs } =
    case ( ground, state, adjacentBombs ) of
        ( _, Blank, _ ) ->
            viewWithNoText Nothing

        ( _, Marked, _ ) ->
            viewWithNoText (Just MyCss.MarkedTile)

        ( Clear, Cleared, n ) ->
            viewWithText (Just MyCss.ClearedTile) (toString n)

        ( Bomb, Cleared, _ ) ->
            viewWithNoText (Just MyCss.DetonatedTile)

        ( _, Detonated, _ ) ->
            viewWithNoText (Just MyCss.DetonatedTile)


viewWithNoText css =
    span ((cssFor css) ++ [ onClick DoClear, onRightClick DoMark ]) []


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


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    Html.Events.onWithOptions
        "contextmenu"
        { defaultOptions | preventDefault = True }
        (Json.succeed message)


showAll tiles =
    List.map showAllRow tiles


showAllRow tiles =
    List.map markTile tiles


markTile tile =
    { tile | state = Marked }


sweepTile tile =
    case tile.ground of
        Bomb ->
            { tile | state = Detonated }

        _ ->
            { tile | state = Cleared }


expose tile =
    { tile | state = Cleared }


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
