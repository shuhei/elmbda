module ComboTests exposing (..)

import Test exposing (..)
import Expect
import String
import Combo exposing (..)


all : Test
all =
    describe "Combo"
        [ test "return" <|
            \() ->
                Expect.equal
                    (parse (return 1) "abc")
                    (Just ( 1, "abc" ))
        , test "failur" <|
            \() ->
                Expect.equal
                    (parse failure "abc")
                    Nothing
        , test "item" <|
            \() ->
                Expect.equal
                    (item "hello")
                    (Just ( 'h', "ello" ))
        , test "andMap" <|
            \() ->
                let
                    p =
                        return (,) <*> item <*> item
                in
                    Expect.equal
                        (parse p "hello")
                        (Just ( ( 'h', 'e' ), "llo" ))
        ]
