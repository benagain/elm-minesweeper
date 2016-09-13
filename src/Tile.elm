module Tile
    exposing
        ( Model
        , Msg
        , view
        , initBomb
        , initClear
        , addBombCount
        , update
        , expose
        , didDetonate
        , isBomb
        )

import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick, defaultOptions)
import HtmlExtras exposing (onRightClick)
import Html.CssHelpers
import MyCss


-- Model


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



-- Init


initBomb =
    { initClear | ground = Bomb }


initClear =
    { ground = Clear, state = Blank, adjacentBombs = 0 }


addBombCount count tile =
    { tile | adjacentBombs = count }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        DoClear ->
            sweepTile model

        DoMark ->
            mark model


sweepTile tile =
    case tile.ground of
        Bomb ->
            { tile | state = Detonated }

        _ ->
            expose tile


mark tile =
    { tile | state = Marked }


expose tile =
    { tile | state = Cleared }


didDetonate tile =
    tile.state == Detonated


isBomb : Model -> Bool
isBomb { ground } =
    case ground of
        Bomb ->
            True

        Clear ->
            False



-- View


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


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
