module View exposing (view)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, defaultOptions)
import HtmlExtras exposing (onRightClick)
import MyCss exposing (..)
import Model exposing (Model, Msg(..), Tile(..))


view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        , style [ ( "width", toString (toFloat (model.square) * 2.4) ++ "em" ) ]
        ]
        (gameView model)


gameView : Model -> List (Html Msg)
gameView model =
    model.tiles |> List.indexedMap tileView


tileView : Int -> ( Tile, Int ) -> Html Msg
tileView index ( tile, bombCount ) =
    case tile of
        CoveredBomb ->
            viewWithNoText2 Nothing index "b"

        CoveredClear ->
            viewWithNoText Nothing index

        MarkedBomb ->
            viewWithNoText (Just MyCss.MarkedTile) index

        MarkedClear ->
            viewWithNoText (Just MyCss.MarkedTile) index

        ExposedClear ->
            viewWithText (Just MyCss.ClearedTile) (toString <| bombCount)

        ExposedBomb ->
            viewWithNoText (Just MyCss.DetonatedTile) index

        Detonated ->
            viewWithText (Just MyCss.DetonatedTile) "!"


viewWithNoText : Maybe CssClasses -> Int -> Html Msg
viewWithNoText css xy =
    span ((cssFor css) ++ [ onClick (DoClear xy), onRightClick (DoMark xy) ]) []


viewWithNoText2 : Maybe CssClasses -> Int -> String -> Html Msg
viewWithNoText2 css xy text =
    span ((cssFor css) ++ [ onClick (DoClear xy), onRightClick (DoMark xy) ]) [ Html.text (text) ]


viewWithText : Maybe a -> String -> Html b
viewWithText css text =
    span (cssFor css) [ Html.text (text) ]


cssFor : Maybe a -> List (Html.Attribute b)
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
