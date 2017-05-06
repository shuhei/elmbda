module App exposing (..)

import Html exposing (Html, text, h1, div, button, textarea)
import Html.Attributes exposing (src, value, disabled, class)
import Html.Events exposing (onInput, onClick)
import Lambda exposing (..)
import LambdaParser exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    , results : List Expression
    }


init : ( Model, Cmd Msg )
init =
    ( { text = "", results = [] }, Cmd.none )



---- UPDATE ----


toList : Maybe a -> List a
toList m =
    case m of
        Just a ->
            [ a ]

        Nothing ->
            []


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
            ( { model | text = text, results = toList <| parseLambda text }
            , Cmd.none
            )

        Reduce ->
            let
                results =
                    case model.results of
                        [] ->
                            []

                        r :: rs ->
                            case reduce r of
                                Ok exp ->
                                    (exp :: model.results)

                                _ ->
                                    model.results
            in
                ( { model | results = results }, Cmd.none )



---- VIEW ----


resultView : Expression -> Html Msg
resultView exp =
    div [ class "result" ] [ text <| printExpression exp ]


view : Model -> Html Msg
view model =
    let
        results =
            if List.isEmpty model.results then
                [ div [ class "result" ] [ text "-" ] ]
            else
                List.map resultView <| List.reverse model.results
    in
        div [ class "container" ]
            [ h1 [] [ text "Elmbda" ]
            , textarea [ onInput TextInput, value model.text ] []
            , div [ class "results" ] results
            , button [ onClick Reduce, disabled (List.isEmpty model.results) ] [ text "Reduce" ]
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
