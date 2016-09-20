module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (span)
import Css.Namespace exposing (namespace)


css =
    (stylesheet << namespace "dreamwriter")
        [ (.) Board
            [ width (em 7)
            , height (em 6)
            , children
                [ span
                    [ backgroundColor grey
                    , width (em 2)
                    , height (em 2)
                    , border (px 2)
                    , borderTopColor (hex "ffffff")
                    , borderStyle solid
                    , borderColor (hex "663399")
                    ]
                ]
            ]
        , (.) ClearedTile
            [ (backgroundColor (hex "ffffff")) |> important ]
        , (.) MarkedTile
            [ (backgroundColor (hex "ffff00")) |> important ]
        , (.) None []
        ]


type CssClasses
    = Board
    | None
    | ClearedTile
    | MarkedTile
    | ExposedBombTile
    | DetonatedTile


grey =
    hex "ccffaa"
