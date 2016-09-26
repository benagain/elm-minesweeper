module Update exposing (update, clearPath)

import Model exposing (..)
import Board
import List.Extra
import List.Extras exposing (updateAt', mapIndices)
import Tuple2 exposing (mapFst)
import Set exposing (Set)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Board.generate )

        NewBoard board ->
            ( Board.toModel board, Cmd.none )

        DoClear xy ->
            ( expose xy model, Cmd.none )

        DoMark xy ->
            ( mark xy model, Cmd.none )


mark : Int -> Model -> Model
mark index model =
    { model
        | tiles = model.tiles |> updateAt' index (mapFst markMe)
    }


markMe : Tile -> Tile
markMe tile =
    case tile of
        MarkedBomb ->
            CoveredBomb

        MarkedClear ->
            CoveredClear

        CoveredBomb ->
            MarkedBomb

        CoveredClear ->
            MarkedClear

        _ ->
            tile


expose : Int -> Model -> Model
expose index model =
    let
        detonated =
            model.tiles
                |> List.Extra.getAt index
                |> Maybe.map (fst >> Board.isBomb)
                |> Maybe.withDefault False

        updateNeighbours =
            if detonated then
                exposeAll
            else
                clearSafePath index model
    in
        { model
            | tiles =
                model.tiles
                    |> updateAt' index (mapFst exposeMe)
                    |> updateNeighbours
        }


exposeAll : TileList -> TileList
exposeAll list =
    List.map (mapFst exposeTile) list


{-| Expose neighours of neighbours that don't have bombs
-}
clearSafePath : Int -> Model -> TileList -> TileList
clearSafePath index model list =
    List.Extras.mapIndices (mapFst exposeTile) (clearPath index model) list


{-| Expose a tile that has been clicked on
-}
exposeMe : Tile -> Tile
exposeMe tile =
    case tile of
        CoveredBomb ->
            Detonated

        other ->
            exposeTile other


{-| Expose a tile that may not have been directly clicked on
-}
exposeTile : Tile -> Tile
exposeTile tile =
    case (tile) of
        CoveredBomb ->
            ExposedBomb

        CoveredClear ->
            ExposedClear

        other ->
            other


clearPath : Int -> Model -> List Int
clearPath index tiles =
    if hasNeighouringBombs index tiles.tiles then
        clearPath' [ index ] tiles []
    else
        []


hasNeighouringBombs : Int -> TileList -> Bool
hasNeighouringBombs index tiles =
    tiles
        |> List.Extra.getAt index
        |> Maybe.map (snd >> ((==) 0))
        |> Maybe.withDefault False


{-| Determine all tiles that have don't neighouring bombs and their immediate surrounding tiles that do
-}
clearPath' : List Int -> Model -> List Int -> List Int
clearPath' maybeZero model alreadyProcessed =
    case maybeZero of
        [] ->
            []

        head :: tail ->
            let
                tileIsZero =
                    hasNeighouringBombs head model.tiles

                toTest =
                    if tileIsZero then
                        head
                            |> Board.surroundingSquare model.square
                            |> Set.fromList
                            |> Set.union (Set.fromList tail)
                            |> (flip Set.diff (Set.fromList alreadyProcessed))
                            |> Set.toList
                    else
                        tail
            in
                head :: (clearPath' toTest model (head :: alreadyProcessed))
