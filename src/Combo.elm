module Combo exposing (..)


type alias Parser a =
    String -> Maybe ( a, String )


return : a -> Parser a
return v =
    \inp -> Just ( v, inp )


failure : Parser a
failure =
    \inp -> Nothing


item : Parser Char
item =
    String.uncons


parse : Parser a -> String -> Maybe ( a, String )
parse p inp =
    p inp


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f p =
    \inp ->
        case parse p inp of
            Nothing ->
                Nothing

            Just ( v, out ) ->
                parse (f v) out


(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) =
    flip andThen


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


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap p pf =
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


(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) =
    flip andMap
