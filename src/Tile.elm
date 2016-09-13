module Tile
    exposing
        ( Model
        , Msg(..)
        , TileState(..)
        , view
        , bombTile
        , clearTile
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
    | Clear Int


type TileState
    = Blank
    | Marked
    | Cleared
    | Detonated


type alias Model =
    ( Ground, TileState )


type Msg
    = DoClear
    | DoMark


bombTile =
    ( Bomb, Blank )


clearTile =
    ( Clear 0, Blank )


update : Msg -> Model -> Model
update msg model =
    case msg of
        DoClear ->
            sweepTile model

        DoMark ->
            markTile model


view : Model -> Html Msg
view tile =
    case tile of
        ( _, Blank ) ->
            viewWithNoText Nothing

        ( _, Marked ) ->
            viewWithNoText (Just MyCss.MarkedTile)

        ( Clear n, Cleared ) ->
            viewWithText (Just MyCss.ClearedTile) (toString n)

        ( Bomb, Cleared ) ->
            viewWithNoText (Just MyCss.DetonatedTile)

        ( _, Detonated ) ->
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


markTile ( ground, state ) =
    ( ground, Marked )


sweepTile ( ground, state ) =
    case ground of
        Bomb ->
            ( Bomb, Detonated )

        _ ->
            ( ground, Cleared )


expose ( ground, state ) =
    ( ground, Cleared )


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
