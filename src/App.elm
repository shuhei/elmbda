module App exposing (..)

import Html exposing (Html, text, div, img, textarea)
import Html.Attributes exposing (src)


type Expression
    = Name String
    | Function String Expression
    | Application Expression Expression


type ParseError
    = UnboundFunctionApplication String


type alias Parser a = String -> Maybe (a, String)


return : a -> Parser a
return v = \inp -> Just (v, inp)


failure : Parser a
failure = \inp -> Nothing


item : Parser Char
item = String.uncons


parse : Parser a -> String -> Maybe (a, String)
parse p inp = p inp


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f p =
    \inp ->
        case parse p inp of
            Nothing -> Nothing
            Just (v, out) -> parse (f v) out

(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) = flip andThen

map : (a -> b) -> Parser a -> Parser b
map f p =
    \inp ->
        case parse p inp of
            Nothing -> Nothing
            Just (v, out) -> Just (f v, out)

(<$>) : Parser a -> (a -> b) -> Parser b
(<$>) = flip map

andMap : Parser a -> Parser (a -> b) -> Parser b
andMap p pf =
    \inp ->
        case parse pf inp of
            Nothing -> Nothing
            Just (f, out1) ->
                case parse p out1 of
                    Nothing -> Nothing
                    Just (v, out2) ->
                        Just (f v, out2)

(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) = flip andMap

substitute : String -> Expression -> Expression -> Expression
substitute from to exp =
    case exp of
        Name name ->
            if name == from then
                to
            else
                exp

        Function arg body ->
            if arg == from then
                exp
            else
                Function arg <| substitute from to body

        Application func arg ->
            Application
                (substitute from to func)
                (substitute from to arg)


apply : Expression -> Expression -> Result ParseError Expression
apply func arg =
    case func of
        Name name ->
            Err <| UnboundFunctionApplication name

        Function name body ->
            Ok <| substitute name arg body

        Application _ _ ->
            Result.andThen
                (\f -> Ok (Application f arg))
                (reduce func)


reduce : Expression -> Result ParseError Expression
reduce exp =
    case exp of
        Name name ->
            Ok exp

        Function arg body ->
            Ok exp

        Application func arg ->
            apply func arg


printExpression : Expression -> String
printExpression exp =
    case exp of
        Name name ->
            name

        Function arg body ->
            "λ" ++ arg ++ "." ++ printExpression body

        Application func arg ->
            "(" ++ printExpression func ++ " " ++ printExpression arg ++ ")"



---- MODEL ----


type alias Model =
    { message : String
    , logo : String
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { message = "Your Elm App is working!", logo = path }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src model.logo ] []
        , div [] [ text model.message ]
        , textarea [] []
        ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
