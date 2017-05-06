module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (..)
import ComboTests
import LambdaTests
import LambdaParserTests


all : Test
all =
    describe "A Test Suite"
        [ ComboTests.all
        , LambdaTests.all
        , LambdaParserTests.all
        ]
