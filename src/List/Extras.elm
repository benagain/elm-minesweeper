module List.Extras exposing (..)

import List.Extra exposing (updateAt)


updateAt' : Int -> (a -> a) -> List a -> List a
updateAt' index update list =
    case List.Extra.updateAt index update list of
        Nothing ->
            list

        Just updated ->
            updated


indexedFilter : (( Int, a ) -> Bool) -> List a -> List a
indexedFilter fn list =
    let
        conditionalCons index x xs' =
            if fn ( index, x ) then
                x :: xs'
            else
                xs'

        -- step : a -> ( Int, List a ) -> ( Int, List a )
        step element ( index, acc ) =
            ( index + 1, conditionalCons index element acc )
    in
        snd (List.foldl step ( 0, [] ) list) |> List.reverse


takeIndices : List Int -> List a -> List a
takeIndices indices xs =
    -- indexedFilter (fst >> (==) (List.length indices)) xs
    takeIndices' 0 indices xs


takeIndices' : Int -> List Int -> List a -> List a
takeIndices' index indices xs =
    case ( indices, xs ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( index' :: otherIndices, head :: tail ) ->
            if index == index' then
                head :: takeIndices' (index + 1) otherIndices tail
            else
                takeIndices' (index + 1) indices tail
