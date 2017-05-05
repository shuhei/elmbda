module App exposing (..)

import Html exposing (Html, text, h1, div, button, textarea)
import Html.Attributes exposing (src, value, disabled)
import Html.Events exposing (onInput, onClick)
import Lambda exposing (..)
import LambdaParser exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    , result : Maybe Expression
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { text = "", result = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | TextInput String
    | Reduce


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput text ->
            ( { model | text = text, result = parseLambda text }
            , Cmd.none
            )

        Reduce ->
            ( { model | result = Maybe.andThen (Result.toMaybe << reduce) model.result }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        result =
            case model.result of
                Nothing ->
                    div [] [ text "-" ]

                Just exp ->
                    div [] [ text <| printExpression exp ]
        isOK =
            case model.result of
                Nothing ->
                    False

                Just _ ->
                    True
    in
        div []
            [ h1 [] [ text "Elmbda" ]
            , textarea [ onInput TextInput, value model.text ] []
            , result
            , button [ onClick Reduce, disabled (not isOK) ] [ text "Reduce" ]
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
