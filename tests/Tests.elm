module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (..)
import ComboTests
import LambdaTests


all : Test
all =
    describe "A Test Suite"
        [ test "App.model.message should be set properly" <|
            \() ->
                Expect.equal (Tuple.first (App.init "../src/logo.svg") |> .message) "Your Elm App is working!"
        , ComboTests.all
        , LambdaTests.all
        ]
