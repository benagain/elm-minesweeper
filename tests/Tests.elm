module Tests exposing (..)

import Test exposing (..)
import BoardTests


all : Test
all =
    describe "minesweeper" [ BoardTests.all ]
