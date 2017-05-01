module Combo exposing (..)

import Char
import Lazy


type alias ParseFn a =
    String -> Maybe ( a, String )


{-| Use Lazy to handle recursive parser.
Stolen from <https://github.com/elm-community/parser-combinators>
-}
type Parser a
    = Parser (ParseFn a)
    | RecursiveParser (Lazy.Lazy (ParseFn a))


return : a -> Parser a
return v =
    Parser <| \inp -> Just ( v, inp )


failure : Parser a
failure =
    Parser <| \inp -> Nothing


lazy : (() -> Parser a) -> Parser a
lazy makeParser =
    RecursiveParser <|
        Lazy.lazy <|
            \() -> parse (makeParser ())


parse : Parser a -> String -> Maybe ( a, String )
parse p inp =
    case p of
        Parser f ->
            f inp

        RecursiveParser f ->
            Lazy.force f <| inp


or : Parser a -> Parser a -> Parser a
or p q =
    Parser <|
        \inp ->
            case parse p inp of
                Nothing ->
                    parse q inp

                x ->
                    x


(<|>) : Parser a -> Parser a -> Parser a
(<|>) =
    or



-- Monad!


(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) p f =
    Parser <|
        \inp ->
            case parse p inp of
                Nothing ->
                    Nothing

                Just ( v, out ) ->
                    parse (f v) out


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
    flip (>>=)



-- Functor!


map : (a -> b) -> Parser a -> Parser b
map f p =
    Parser <|
        \inp ->
            case parse p inp of
                Nothing ->
                    Nothing

                Just ( v, out ) ->
                    Just ( f v, out )


(<$>) : Parser a -> (a -> b) -> Parser b
(<$>) =
    flip map



-- Applicative!


(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) pf p =
    Parser <|
        \inp ->
            case parse pf inp of
                Nothing ->
                    Nothing

                Just ( f, out1 ) ->
                    case parse p out1 of
                        Nothing ->
                            Nothing

                        Just ( v, out2 ) ->
                            Just ( f v, out2 )


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap =
    flip (<*>)


(<*) : Parser a -> Parser b -> Parser a
(<*) pa pb =
    return (\x y -> x) <*> pa <*> pb


(*>) : Parser a -> Parser b -> Parser b
(*>) pa pb =
    return (\x y -> y) <*> pa <*> pb



-- Parsers


item : Parser Char
item =
    Parser String.uncons


sat : (Char -> Bool) -> Parser Char
sat p =
    item
        >>= (\x ->
                if p x then
                    return x
                else
                    failure
            )


char : Char -> Parser Char
char c =
    sat ((==) c)


alpha : Parser Char
alpha =
    sat (\x -> Char.isLower x || Char.isUpper x)


string : String -> Parser String
string s =
    case String.uncons s of
        Just ( x, xs ) ->
            return String.cons <*> char x <*> string xs

        Nothing ->
            failure


many : Parser a -> Parser (List a)
many p =
    Parser <|
        \inp ->
            case parse p inp of
                Just ( v, out ) ->
                    parse (return ((::) v) <*> many p) out

                Nothing ->
                    Just ( [], inp )


many1 : Parser a -> Parser (List a)
many1 p =
    return (::) <*> p <*> many p


manyS : Parser Char -> Parser String
manyS p =
    Parser <|
        \inp ->
            case parse p inp of
                Just ( v, out ) ->
                    parse (return ((String.cons) v) <*> manyS p) out

                Nothing ->
                    Just ( "", inp )


manyS1 : Parser Char -> Parser String
manyS1 p =
    return String.cons <*> p <*> manyS p
