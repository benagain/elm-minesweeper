module Main exposing (..)

import Html.App as App
import List
import Random.Extra
import Random
import Set
import View exposing (view)
import Model exposing (..)
import Update exposing (update)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Init


init : ( Model, Cmd Msg )
init =
    ( Model 0 [], Random.generate NewBoard (randomBoard 5) )


randomBoard : Int -> Random.Generator (List Tile)
randomBoard size =
    Random.list (size ^ 2) bombFlip


bombFlip : Random.Generator Tile
bombFlip =
    Random.map
        (\b ->
            if b then
                CoveredBomb
            else
                CoveredClear
        )
        (Random.Extra.oneIn 4)
