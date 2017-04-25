module App exposing (..)

import Html exposing (Html, text, div, img, textarea)
import Html.Attributes exposing (src)


type Expression
    = Name String
    | Function String Expression
    | Application Expression Expression


type ParseError
    = UnboundFunctionApplication String


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
