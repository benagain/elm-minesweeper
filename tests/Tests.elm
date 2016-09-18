module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Game exposing (..)
import Set exposing (Set)


all : Test
all =
    describe "Game happho"
        [ test "top-left" <|
            \() ->
                happho 3 4 |> setContains 0
        , test "top-middle" <|
            \() ->
                happho 3 4 |> setContains 1
        , test "top-right" <|
            \() ->
                happho 3 4 |> setContains 2
        , test "bottom-left" <|
            \() ->
                happho 3 4 |> setContains 6
        , test "bottom-middle" <|
            \() ->
                happho 3 4 |> setContains 7
        , test "bottom-right" <|
            \() ->
                happho 3 4 |> setContains 8
        , test "centre" <|
            \() ->
                happho 3 4 |> Set.toList |> Expect.equal [ 0, 1, 2, 3, 5, 6, 7, 8 ]
        , test "off-left" <|
            \() ->
                happho 3 3 |> Set.toList |> Expect.equal [ 0, 1, 4, 6, 7 ]
        , test "off-top" <|
            \() ->
                happho 3 1 |> Set.toList |> Expect.equal [ 0, 2, 3, 4, 5 ]
        , test "off-right" <|
            \() ->
                happho 3 5 |> Set.toList |> Expect.equal [ 0, 2, 3, 4, 5 ]
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
