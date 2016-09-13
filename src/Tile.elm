module Tile exposing (Model, view, bombTile, clearTile, Msg, update, Msg(..), showAll, showTile)

import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.CssHelpers
import MyCss


type Ground
    = Bomb
    | Clear


type TileState
    = Blank
    | Marked
    | Cleared Int


type alias Model =
    ( Ground, TileState )


type Msg
    = DoClear
    | DoMark


update : Msg -> Model -> Model
update msg model =
    model


bombTile =
    ( Bomb, Blank )


clearTile =
    ( Clear, Blank )


view : Model -> Html Msg
view tile =
    case tile of
        ( _, Blank ) ->
            viewWithNoText Nothing

        ( _, Marked ) ->
            viewWithNoText (Just MyCss.MarkedTile)

        ( _, Cleared n ) ->
            viewWithText (Just MyCss.ClearedTile) (toString n)


viewWithNoText css =
    span ((cssFor css) ++ [ onClick DoClear ]) []


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


showAll tiles =
    List.map showAllRow tiles


showAllRow tiles =
    List.map showTile tiles


showTile tile =
    case tile of
        ( a, b ) ->
            ( a, Marked )


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
