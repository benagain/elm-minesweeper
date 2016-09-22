module Tests exposing (..)

import Test exposing (..)
import BoardTests
import ListTests


all : Test
all =
    describe "minesweeper"
        [ BoardTests.all
        , ListTests.all
        ]
