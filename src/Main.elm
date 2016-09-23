module Main exposing (..)

import Html.App as App
import View exposing (view)
import Model exposing (Model, Msg)
import Update exposing (update)
import Board


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( Model 0 [], Board.generate )
