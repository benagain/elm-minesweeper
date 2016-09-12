module Main exposing (..)

import Html exposing (Html, button, div, text, span)
import Html.CssHelpers
import Html.App as App
import MyCss exposing (..)
import List


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
        [ class [ MyCss.Tile ]
        ]
        (gameView tiles)


gameView : List (List Tile) -> List (Html.Html msg)
gameView tiles =
    tiles
        |> List.foldr (++) []
        |> List.map tileView


type Ground
    = Bomb
    | Clear


type TileState
    = Blank
    | Marked
    | Cleared Int


type alias Tile =
    ( Ground, TileState )


isABomb =
    ( Bomb, Blank )


notABomb =
    ( Clear, Blank )


tileView : Tile -> Html a
tileView tile =
    span [] [ Html.text (tileText tile) ]


tileText : ( Ground, TileState ) -> String
tileText tile =
    case tile of
        ( _, Blank ) ->
            " "

        ( _, Marked ) ->
            "?"

        ( _, Cleared n ) ->
            toString n


tiles : List (List Tile)
tiles =
    [ [ notABomb, notABomb, notABomb ]
    , [ notABomb, ( Clear, Cleared 3 ), notABomb ]
    , [ notABomb, ( Bomb, Marked ), notABomb ]
    ]
