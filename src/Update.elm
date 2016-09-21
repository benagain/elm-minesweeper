module Update exposing (update)

import Model exposing (..)
import Board
import List.Extra
import List.Extras exposing (updateAt')
import Tuple2 exposing (mapFst)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Board.generate )

        NewBoard board ->
            ( Board.toModel board, Cmd.none )

        DoClear xy ->
            ( expose xy model, Cmd.none )

        _ ->
            Debug.crash "TODO"


expose : Int -> Model -> Model
expose index model =
    let
        tile =
            model.tiles |> List.Extra.getAt index

        detonated =
            tile
                |> Maybe.map (fst >> Board.isBomb)
                |> Maybe.withDefault False

        updateTiles =
            model.tiles
                |> updateAt' index (mapFst exposeMe)
                |> if detonated then
                    List.map (mapFst exposeTile)
                   else
                    identity
    in
        { model
            | tiles = updateTiles
        }


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
