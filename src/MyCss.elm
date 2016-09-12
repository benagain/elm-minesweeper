module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (span)
import Css.Namespace exposing (namespace)


css =
    (stylesheet << namespace "dreamwriter")
        [ (.) Tile
            [ width (em 6)
            , height (em 6)
            , children
                [ span
                    [ backgroundColor grey
                    , width (em 2)
                    , height (em 2)
                    , border (px 2)
                    , borderStyle solid
                    , borderColor (hex "663399")
                    ]
                ]
            ]
        ]


type CssClasses
    = Tile


grey =
    hex "ccffaa"
