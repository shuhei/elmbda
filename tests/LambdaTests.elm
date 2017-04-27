module LambdaTests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (..)
import Lambda exposing (..)


all : Test
all =
    describe "Lambda"
        [ describe "reduce"
            [ test "root name is not reduced" <|
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
        , describe "printExpression"
            [ test "name" <|
                \() ->
                    Expect.equal
                        (printExpression <| Name "foo")
                        "foo"
            , test "function" <|
                \() ->
                    Expect.equal
                        (printExpression <| Function "foo" (Name "foo"))
                        "λfoo.foo"
            , test "application" <|
                \() ->
                    Expect.equal
                        (printExpression <|
                            Application
                                (Function "s" <| Application (Name "s") (Name "s"))
                                (Function "x" <| Name "x")
                        )
                        "(λs.(s s) λx.x)"
            ]
        ]
