module Combo exposing (..)


type alias Parser a =
    String -> Maybe ( a, String )


return : a -> Parser a
return v =
    \inp -> Just ( v, inp )


failure : Parser a
failure =
    \inp -> Nothing


parse : Parser a -> String -> Maybe ( a, String )
parse p inp =
    p inp

or : Parser a -> Parser a -> Parser a
or p q =
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
    String.uncons

char : Char -> Parser Char
char c =
    item >>= (\x -> if x == c then return c else failure)
