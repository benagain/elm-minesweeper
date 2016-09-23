module BoardTests exposing (..)

import Test exposing (..)
import Expect
import String
import Board
import Set exposing (Set)
import Model exposing (Tile(..))
import Update


all : Test
all =
    describe "board" [ happo, clearPath ]


happo : Test
happo =
    describe "Game happo"
        [ test "top-left" <|
            \() ->
                Board.happo 3 4 |> setContains 0
        , test "top-middle" <|
            \() ->
                Board.happo 3 4 |> setContains 1
        , test "top-right" <|
            \() ->
                Board.happo 3 4 |> setContains 2
        , test "bottom-left" <|
            \() ->
                Board.happo 3 4 |> setContains 6
        , test "bottom-middle" <|
            \() ->
                Board.happo 3 4 |> setContains 7
        , test "bottom-right" <|
            \() ->
                Board.happo 3 4 |> setContains 8
        , test "centre" <|
            \() ->
                Board.happo 3 4 |> Set.toList |> Expect.equal [ 0, 1, 2, 3, 5, 6, 7, 8 ]
        , test "off-left" <|
            \() ->
                Board.happo 3 3 |> Set.toList |> Expect.equal [ 0, 1, 4, 6, 7 ]
        , test "off-top" <|
            \() ->
                Board.happo 3 1 |> Set.toList |> Expect.equal [ 0, 2, 3, 4, 5 ]
        , test "off-right" <|
            \() ->
                Board.happo 3 5 |> Set.toList |> Expect.equal [ 1, 2, 4, 7, 8 ]
        ]


setContains : comparable -> Set comparable -> Expect.Expectation
setContains expected actual =
    Set.toList actual |> listContains expected


listContains : a -> List a -> Expect.Expectation
listContains expected actual =
    if 0 < (List.filter ((==) expected) actual |> List.length) then
        Expect.pass
    else
        [ toString actual
        , "╷"
        , "| Expect.listContains"
        , "╵"
        , toString expected
        ]
            |> String.join "\n"
            |> Expect.fail


sampleBoard =
    Board.toModel
        [ CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredBomb
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredBomb
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredBomb
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredClear
        , CoveredBomb
        ]


debugSampleBoard =
    Debug.log "sampleBoard" (sampleBoard.tiles |> List.map snd)



-- [
--   0,0,0,1,1,1,0,
--   0,0,0,1,x,1,0,
--   0,1,1,2,1,1,0,
--   1,2,x,1,0,0,0,
--   x,2,1,1,0,0,0,
--   1,1,0,0,0,1,1,
--   0,0,0,0,0,1,x
-- ]


clearPath =
    describe "clearPath"
        [ test "with path" <|
            \() ->
                Update.clearPath sampleBoard.square [ 0 ] sampleBoard.tiles []
                    |> List.sort
                    |> Expect.equal [ 0, 1, 2, 7, 8, 9, 14 ]
        , test "initial not zero returns empty" <|
            \() ->
                Update.clearPath sampleBoard.square [ 3 ] sampleBoard.tiles []
                    |> List.sort
                    |> Expect.equal []
        , test "path 2" <|
            \() ->
                Update.clearPath sampleBoard.square [ 32 ] sampleBoard.tiles []
                    |> List.sort
                    |> Expect.equal [ 6, 13, 20, 25, 26, 27, 32, 33, 34, 37, 38, 39, 42, 43, 44, 45, 46 ]
        ]
