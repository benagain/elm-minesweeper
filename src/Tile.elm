module Tile exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import MyCss


type Ground
    = Bomb
    | Clear


type TileState
    = Blank
    | Marked
    | Cleared Int


type alias Tile =
    ( Ground, TileState )


tileView : Tile -> Html a
tileView tile =
    case tile of
        ( _, Blank ) ->
            tileyView2 Nothing

        ( _, Marked ) ->
            tileyView2 (Just MyCss.MarkedTile)

        ( _, Cleared n ) ->
            tileyView (Just MyCss.ClearedTile) (toString n)


tileyView2 : Maybe MyCss.CssClasses -> Html a
tileyView2 customCss =
    let
        cssList =
            mapWithDefault (\css -> [ class [ css ] ]) [] customCss
    in
        span cssList []


tileyView : Maybe MyCss.CssClasses -> String -> Html a
tileyView customCss text =
    let
        cssList =
            mapWithDefault (\css -> [ class [ css ] ]) [] customCss
    in
        span cssList [ Html.text (text) ]


mapWithDefault : (a -> b) -> b -> Maybe a -> b
mapWithDefault fn b a =
    case a of
        Nothing ->
            b

        Just a ->
            fn a


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
