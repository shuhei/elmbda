module LambdaParserTests exposing (..)

import Test exposing (..)
import Expect
import Combo exposing (..)
import LambdaParser exposing (..)
import Lambda exposing (..)


all : Test
all =
    describe "LabmdaParser"
        [ test "name" <|
            \() ->
                Expect.equal
                    (parse expression "abc")
                    (Just ( Name "abc", "" ))
        , test "function" <|
            \() ->
                Expect.equal
                    (parse expression "λfoo.foo")
                    (Just ( Function "foo" (Name "foo"), "" ))
        , test "application" <|
            \() ->
                Expect.equal
                    (parse expression "(f x)")
                    (Just ( Application (Name "f") (Name "x"), "" ))
        , test "combined" <|
            \() ->
                Expect.equal
                    (parse expression "λf.(f x)")
                    (Just ( Function "f" (Application (Name "f") (Name "x")), "" ))
        , test "nested" <|
            \() ->
                Expect.equal
                    (parse expression "λf.λx.((f f) (x x))")
                    (Just ( Function "f" (Function "x" (Application (Application (Name "f") (Name "f")) (Application (Name "x") (Name "x")))), "" ))
        ]
