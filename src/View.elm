module View exposing (view)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, defaultOptions)
import Html.Extras exposing (onRightClick)
import MyCss as Css exposing (CssClasses)
import Model exposing (Model, Msg(..), Tile(..))


view : Model -> Html Msg
view model =
    div
        [ class [ Css.Board ]
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
            clickableVisible Css.None index (toString bombCount)

        CoveredClear ->
            clickableVisible Css.None index (toString bombCount)

        MarkedBomb ->
            clickable Css.MarkedTile index

        MarkedClear ->
            clickable Css.MarkedTile index

        ExposedClear ->
            inert Css.ClearedTile (toString <| bombCount)

        ExposedBomb ->
            inert Css.ExposedBombTile ""

        Detonated ->
            inert Css.DetonatedTile "!"


clickable : CssClasses -> Int -> Html Msg
clickable css index =
    span
        [ class [ css ]
        , onClick (DoClear index)
        , onRightClick (DoMark index)
        ]
        []


clickableVisible : CssClasses -> Int -> String -> Html Msg
clickableVisible css index text =
    span
        [ class [ css ]
        , onClick (DoClear index)
        , onRightClick (DoMark index)
        ]
        [ Html.text (text) ]


inert : CssClasses -> String -> Html b
inert css text =
    span
        [ class [ css ] ]
        [ Html.text (text) ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
