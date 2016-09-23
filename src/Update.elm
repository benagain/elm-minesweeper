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

        bombCount =
            tile
                |> Maybe.map snd
                |> Maybe.withDefault 0

        updateTiles =
            model.tiles
                |> updateAt' index (mapFst exposeMe)
                |> if detonated then
                    List.map (mapFst exposeTile)
                   else if bombCount == 0 then
                    exposeTiles model.square index
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


exposeTiles : Int -> Int -> TileList -> TileList
exposeTiles boardSize index tiles =
    -- List.Extras.mapIndices (mapFst exposeTile) (Board.fourDirections model.square index)
    case tiles |> List.Extra.getAt index of
        Just ( ExposedClear, _ ) ->
            let
                happo =
                    (Board.fourDirections boardSize index)

                toClear =
                    (clearPath boardSize index tiles)
            in
                List.Extras.mapIndices (mapFst exposeTile) toClear tiles

        _ ->
            tiles


clearPath : Int -> Int -> TileList -> List Int
clearPath boardSize index tiles =
    if hasNeighouringBombs index tiles then
        clearPath' boardSize [ index ] tiles [] []
    else
        []


hasNeighouringBombs : Int -> TileList -> Bool
hasNeighouringBombs index tiles =
    tiles
        |> List.Extra.getAt index
        |> Maybe.map (snd >> ((==) 0))
        |> Maybe.withDefault False


clearPath' : Int -> List Int -> TileList -> List Int -> List Int -> List Int
clearPath' boardSize maybeZero tiles soFar alreadyProcessed =
    let
        d =
            Debug.log "clearPath" (toString (maybeZero) ++ " (" ++ toString (soFar) ++ ")")
    in
        case maybeZero of
            [] ->
                []

            head :: tail ->
                let
                    happo =
                        Debug.log "happo"
                            (Board.fourDirections boardSize head
                             -- |> List.filter ((==) 0)
                            )

                    isZero =
                        hasNeighouringBombs head tiles

                    set =
                        Set.fromList tail

                    surrounding =
                        (happo)
                            |> Set.fromList
                            |> Set.union set
                            |> (flip Set.diff (Set.fromList soFar))
                            |> Set.toList

                    xsf =
                        Debug.log "surroundingi" (List.Extras.takeIndices surrounding tiles)

                    toTest =
                        Debug.log "sofar"
                            ((happo)
                                |> Set.fromList
                                |> Set.union set
                                |> (flip Set.diff (Set.fromList alreadyProcessed))
                                |> Set.toList
                            )

                    -- yab = Debug.log "surrounding2" (List.Extras.indexedFilter
                in
                    Debug.log "clearPath ->"
                        (if isZero then
                            let
                                dd =
                                    Debug.log "clearPath..." ("new 0 " ++ toString (surrounding))
                            in
                                head
                                    :: (clearPath' boardSize
                                            toTest
                                            tiles
                                            (surrounding)
                                            (head :: alreadyProcessed)
                                       )
                         else
                            let
                                dd =
                                    Debug.log "clearPath..." "nonZero"
                            in
                                head :: (clearPath' boardSize tail tiles soFar (head :: alreadyProcessed))
                        )



-- (let
--     dd =
--         Debug.log "clearPath..." ("new 0 " ++ toString (surrounding))
--  in
--     head
--         :: (clearPath' boardSize
--                 surrounding
--                 []
--                 tiles
--                 (head :: soFar)
--            )
-- )
