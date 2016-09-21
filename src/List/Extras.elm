module List.Extras exposing (..)

import List.Extra exposing (updateAt)


updateAt' : Int -> (a -> a) -> List a -> List a
updateAt' index update list =
    case List.Extra.updateAt index update list of
        Nothing ->
            list

        Just updated ->
            updated
