port module Stylesheets exposing (..)

import Css.File exposing (..)
import MyCss as Homepage
import Html exposing (div)
import Html.App as Html


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "style.css", compile [ Homepage.css ] ) ]


main : Program Never
main =
    Html.program
        { init = ( (), files cssFiles )
        , view = \_ -> (div [] [])
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
