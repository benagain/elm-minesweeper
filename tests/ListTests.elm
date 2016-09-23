module ListTests exposing (..)

import Test exposing (..)
import Expect
import String
import List.Extras


all : Test
all =
    describe "List.Extras"
        [ indexedFilter
        , takeIndices
        , mapIndices
        ]


indexedFilter : Test
indexedFilter =
    describe "List.Extras.indexedFilter"
        [ test "all true returns same list" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.indexedFilter (\( a, b ) -> True)
                    |> Expect.equal [ 1, 2, 3, 4 ]
        , test "all false returns empty list" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.indexedFilter (\( a, b ) -> False)
                    |> Expect.equal []
        , test "filter first item returns that item" <|
            \() ->
                [ "a", "b", "c" ]
                    |> List.Extras.indexedFilter (\( a, b ) -> a == 0)
                    |> Expect.equal [ "a" ]
        , test "filter second item returns that item" <|
            \() ->
                [ "a", "b", "c" ]
                    |> List.Extras.indexedFilter (\( a, b ) -> a == 1)
                    |> Expect.equal [ "b" ]
        , test "filter event items returns those items" <|
            \() ->
                [ "a", "b", "c", "d" ]
                    |> List.Extras.indexedFilter (\( a, b ) -> a % 2 == 0)
                    |> Expect.equal [ "a", "c" ]
        ]


takeIndices : Test
takeIndices =
    describe "List.Extra.takeIndices"
        [ test "no indices returns empty list" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.takeIndices []
                    |> Expect.equal []
        , test "single element with index 0 returns self" <|
            \() ->
                [ 1 ]
                    |> List.Extras.takeIndices [ 0 ]
                    |> Expect.equal [ 1 ]
        , test "two elements with index 0 returns first" <|
            \() ->
                [ 1, 2 ]
                    |> List.Extras.takeIndices [ 0 ]
                    |> Expect.equal [ 1 ]
        , test "two elements with index 1 returns last" <|
            \() ->
                [ 1, 2 ]
                    |> List.Extras.takeIndices [ 1 ]
                    |> Expect.equal [ 2 ]
        , test "two elements with both indices returns self" <|
            \() ->
                [ 1, 2 ]
                    |> List.Extras.takeIndices [ 0, 1 ]
                    |> Expect.equal [ 1, 2 ]
        , test "returns evens specified elements" <|
            \() ->
                [ 1, 2, 3, 4, 5, 6 ]
                    |> List.Extras.takeIndices [ 1, 4 ]
                    |> Expect.equal [ 2, 5 ]
        ]


mapIndices : Test
mapIndices =
    describe "List.Extra.mapIndices"
        [ test "no indices returns original list" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.mapIndices (always 0) []
                    |> Expect.equal [ 1, 2, 3, 4 ]
        , test "all indices returns all map" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.mapIndices (always 0) [ 0, 1, 2, 3 ]
                    |> Expect.equal [ 0, 0, 0, 0 ]
        , test "inner indices returns mapped inners" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.mapIndices (always 0) [ 1, 2 ]
                    |> Expect.equal [ 1, 0, 0, 4 ]
        , test "outer indices returns mapped outers" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> List.Extras.mapIndices (always 0) [ 0, 3 ]
                    |> Expect.equal [ 0, 2, 3, 0 ]
        ]
