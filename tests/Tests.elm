module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Board exposing (happo)
import Set exposing (Set)


all : Test
all =
    describe "Game happo"
        [ test "top-left" <|
            \() ->
                happo 3 4 |> setContains 0
        , test "top-middle" <|
            \() ->
                happo 3 4 |> setContains 1
        , test "top-right" <|
            \() ->
                happo 3 4 |> setContains 2
        , test "bottom-left" <|
            \() ->
                happo 3 4 |> setContains 6
        , test "bottom-middle" <|
            \() ->
                happo 3 4 |> setContains 7
        , test "bottom-right" <|
            \() ->
                happo 3 4 |> setContains 8
        , test "centre" <|
            \() ->
                happo 3 4 |> Set.toList |> Expect.equal [ 0, 1, 2, 3, 5, 6, 7, 8 ]
        , test "off-left" <|
            \() ->
                happo 3 3 |> Set.toList |> Expect.equal [ 0, 1, 4, 6, 7 ]
        , test "off-top" <|
            \() ->
                happo 3 1 |> Set.toList |> Expect.equal [ 0, 2, 3, 4, 5 ]
        , test "off-right" <|
            \() ->
                happo 3 5 |> Set.toList |> Expect.equal [ 1, 2, 4, 7, 8 ]
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
