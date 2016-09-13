module HtmlExtras exposing (onRightClick)

import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (defaultOptions)
import Json.Decode as Json exposing ((:=))


onRightClick : msg -> Html.Attribute msg
onRightClick message =
    Html.Events.onWithOptions
        "contextmenu"
        { defaultOptions | preventDefault = True }
        (Json.succeed message)
