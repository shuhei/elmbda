module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "App.model.message should be set properly" <|
            \() ->
                Expect.equal (Tuple.first (App.init "../src/logo.svg") |> .message) "Your Elm App is working!"
        , test "root name is not reduced" <|
            \() ->
                Expect.equal
                    (reduce <| Name "a")
                    (Ok <| Name "a")
        , test "root function is not reduced" <|
            \() ->
                Expect.equal
                    (reduce <|
                        Function "foo" (Name "foo")
                    )
                    (Ok <| Function "foo" (Name "foo"))
        , test "reduce identity application" <|
            \() ->
                Expect.equal
                    (reduce <|
                        Application
                            (Function "a" <| Name "a")
                            (Name "hello")
                    )
                    (Ok <| Name "hello")
        , test "reduce constant application" <|
            \() ->
                Expect.equal
                    (reduce <|
                        Application
                            (Function "_" <| Name "a")
                            (Name "hello")
                    )
                    (Ok <| Name "a")
        , test "reduce identity application" <|
            \() ->
                Expect.equal
                    (reduce <|
                        Application
                            (Function "x" <| Name "x")
                            (Name "hello")
                    )
                    (Ok <| Name "hello")
        , test "application on unbound function" <|
            \() ->
                Expect.equal
                    (reduce <|
                        Application
                            (Name "foo")
                            (Name "bar")
                    )
                    (Err <| UnboundFunctionApplication "foo")
        ]
