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
        , test "item" <|
            \() ->
                Expect.equal
                    (parse item "hello")
                    (Just ( 'h', "ello" ))
        , test "char ok" <|
            \() ->
                Expect.equal
                    (parse (char 'h') "hello")
                    (Just ( 'h', "ello" ))
        , test "char ng" <|
            \() ->
                Expect.equal
                    (parse (char 'z') "hello")
                    Nothing
        , test "alpha ok" <|
            \() ->
                Expect.equal
                    (parse alpha "foo")
                    (Just ( 'f', "oo" ))
        , test "alpha ng" <|
            \() ->
                Expect.equal
                    (parse alpha "123")
                    Nothing
        , test "many" <|
            \() ->
                Expect.equal
                    (parse (many alpha) "foo bar")
                    (Just ( ['f', 'o', 'o'], " bar"))
        , test "many empty" <|
            \() ->
                Expect.equal
                    (parse (many alpha) "123")
                    (Just ( [], "123"))
        , test "many1 ok" <|
            \() ->
                Expect.equal
                    (parse (many1 alpha) "foo bar")
                    (Just ( ['f', 'o', 'o'], " bar"))
        , test "many1 ng" <|
            \() ->
                Expect.equal
                    (parse (many1 alpha) "123")
                    Nothing
        , test "manyS" <|
            \() ->
                Expect.equal
                    (parse (manyS alpha) "foo bar")
                    (Just ( "foo", " bar"))
        , test "manyS empty" <|
            \() ->
                Expect.equal
                    (parse (manyS alpha) "123")
                    (Just ( "", "123"))
        , test "manyS1 ok" <|
            \() ->
                Expect.equal
                    (parse (manyS1 alpha) "foo bar")
                    (Just ( "foo", " bar"))
        , test "manyS1 ng" <|
            \() ->
                Expect.equal
                    (parse (manyS1 alpha) "123")
                    Nothing
        ]
