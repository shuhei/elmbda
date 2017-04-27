module ComboTests exposing (..)

import Test exposing (..)
import Expect
import String
import Char
import Combo exposing (..)


all : Test
all =
    describe "Combo"
        [ test "return" <|
            \() ->
                Expect.equal
                    (parse (return 1) "abc")
                    (Just ( 1, "abc" ))
        , test "failure" <|
            \() ->
                Expect.equal
                    (parse failure "abc")
                    Nothing
        , test "<|> 1" <|
            \() ->
                Expect.equal
                    (parse (item <|> return 'z') "abc")
                    (Just ( 'a', "bc" ))
        , test "<|> 2" <|
            \() ->
                Expect.equal
                    (parse (failure <|> item) "abc")
                    (Just ( 'a', "bc" ))
        , test "<|> 3" <|
            \() ->
                Expect.equal
                    (parse (failure <|> failure) "abc")
                    Nothing
        , test "item" <|
            \() ->
                Expect.equal
                    (parse item "hello")
                    (Just ( 'h', "ello" ))
        , test "map" <|
            \() ->
                Expect.equal
                    (parse (item <$> Char.toUpper) "foo")
                    (Just ('F', "oo"))
        , test "<*>" <|
            \() ->
                Expect.equal
                    (parse (return (,) <*> item <*> item) "hello")
                    (Just (( 'h', 'e' ), "llo" ))
        , test "<*" <|
            \() ->
                Expect.equal
                    (parse (item *> item) "hello")
                    (Just ('e', "llo"))
        , test "*>" <|
            \() ->
                Expect.equal
                    (parse (item <* item) "hello")
                    (Just ('h', "llo"))
        ]
