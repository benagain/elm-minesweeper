module Tile exposing (Model, view, bombTile, clearTile, Msg, update, Msg(..), showAll, showTile)

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
    | Cleared Int


type alias Model =
    ( Ground, TileState )


type Msg
    = DoClear
    | DoMark


bombTile =
    ( Bomb, Blank )


clearTile =
    ( Clear, Blank )


update : Msg -> Model -> Model
update msg model =
    case msg of
        DoClear ->
            model

        DoMark ->
            showTile model


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



---onWithOptions "contextmenu" { defaultOptions | preventDefault = True } Json.Decode.value (\_ -> Signal.message address message)


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
