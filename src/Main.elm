module Main exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List
import Tile exposing (..)


-- import Css exposing (..)
-- import Html.Events exposing (onClick)


main : Program Never
main =
    App.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    Int


model : Model
model =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


{ id, class, classList } =
    Html.CssHelpers.withNamespace "dreamwriter"
view : Model -> Html Msg
view model =
    div
        [ class [ MyCss.Board ]
        ]
        (gameView tiles)


gameView : List (List Tile) -> List (Html.Html msg)
gameView tiles =
    tiles
        |> List.foldr (++) []
        |> List.map tileView


isABomb =
    ( Bomb, Blank )


notABomb =
    ( Clear, Blank )


tiles : List (List Tile)
tiles =
    [ [ notABomb, ( Clear, Cleared 1 ), ( Bomb, Marked ) ]
    , [ ( Clear, Cleared 2 ), ( Clear, Cleared 3 ), notABomb ]
    , [ notABomb, ( Bomb, Marked ), notABomb ]
    ]
