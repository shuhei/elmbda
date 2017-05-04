module App exposing (..)

import Html exposing (Html, text, h1, div, img, textarea)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onInput)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput text ->
            ( { model | text = text, result = parseLambda text }
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
                    div [] [ text <| toString exp ]
    in
        div []
            [ h1 [] [ text "Elmbda" ]
            , textarea [ onInput TextInput, value model.text ] []
            , result
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
