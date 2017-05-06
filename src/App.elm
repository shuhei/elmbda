module App exposing (..)

import Html exposing (Html, text, h1, div, button, textarea)
import Html.Attributes exposing (src, value, disabled, class)
import Html.Events exposing (onInput, onClick)
import Lambda exposing (..)
import LambdaParser exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    , result : Maybe Expression
    }


init : ( Model, Cmd Msg )
init =
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
                    "-"

                Just exp ->
                    printExpression exp
        isOK =
            case model.result of
                Nothing ->
                    False

                Just _ ->
                    True
    in
        div [ class "container" ]
            [ h1 [] [ text "Elmbda" ]
            , textarea [ onInput TextInput, value model.text ] []
            , div [ class "result" ] [ text result ]
            , button [ onClick Reduce, disabled (not isOK) ] [ text "Reduce" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
