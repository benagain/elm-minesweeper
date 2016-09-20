module Update exposing (update)

import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Model.generateCommand )

        NewBoard board ->
            ( toModel board, Cmd.none )

        DoClear xy ->
            ( expose xy model, Cmd.none )

        _ ->
            Debug.crash "TODO"


expose : Int -> Model -> Model
expose xy model =
    let
        detonated =
            model.tiles
                |> List.drop xy
                |> List.head
                |> Maybe.map (fst >> isBomb)
                |> Maybe.withDefault False
    in
        case detonated of
            True ->
                { model
                    | tiles =
                        replaceAt (onFirst exposeMe) xy model.tiles
                            |> List.map (onFirst exposeTile)
                }

            False ->
                { model | tiles = replaceAt (onFirst exposeMe) xy model.tiles }


onFirst : (a -> a) -> ( a, b ) -> ( a, b )
onFirst fn a =
    ( fn <| fst a, snd a )


replaceAt : (a -> a) -> Int -> List a -> List a
replaceAt fn idx list =
    case ( list, idx ) of
        ( [], _ ) ->
            []

        ( head :: tail, 0 ) ->
            fn head :: tail

        ( head :: tail, i ) ->
            head :: replaceAt fn (i - 1) tail


exposeMe : Tile -> Tile
exposeMe tile =
    case tile of
        CoveredBomb ->
            Detonated

        other ->
            exposeTile other


exposeTile : Tile -> Tile
exposeTile tile =
    case (tile) of
        CoveredBomb ->
            ExposedBomb

        CoveredClear ->
            ExposedClear

        other ->
            other
