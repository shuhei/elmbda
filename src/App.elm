module App exposing (..)

import Html exposing (Html, text, h1, h2, div, ul, ol, li, button, textarea)
import Html.Attributes exposing (src, value, disabled, class)
import Html.Events exposing (onInput, onClick)
import Lambda exposing (..)
import LambdaParser exposing (..)


---- MODEL ----


type alias Model =
    { text : String
    , results : List Expression
    , done : Bool
    , examples : List String
    }


examples : List String
examples =
    [ "(\\s.(s s) \\x.x)"
    , "(\\x.(\\f.\\a.(f a) x) foo)"
    ]


init : ( Model, Cmd Msg )
init =
    ( { text = "", results = [], done = False, examples = examples }, Cmd.none )



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
    | ShowExample String


setText : Model -> String -> Model
setText model text =
    { model | text = text, done = False, results = toList <| parseLambda text }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TextInput text ->
            ( setText model text, Cmd.none )

        ShowExample example ->
            ( setText model example, Cmd.none )

        Reduce ->
            let
                updated =
                    case model.results of
                        [] ->
                            model

                        r :: rs ->
                            case reduce r of
                                Ok exp ->
                                    if exp == r then
                                        { model | done = True }
                                    else
                                        { model | results = exp :: model.results }

                                _ ->
                                    { model | done = True }
            in
                ( updated, Cmd.none )



---- VIEW ----


resultView : Expression -> Html Msg
resultView exp =
    div [ class "result" ] [ text <| printExpression exp ]


exampleView : String -> Html Msg
exampleView example =
    li [ class "example", onClick <| ShowExample example ] [ text example ]


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
            , button [ onClick Reduce, disabled (model.done || List.isEmpty model.results) ] [ text "Reduce" ]
            , h2 [] [ text "Examples" ]
            , ol [ class "examples" ] <| List.map exampleView model.examples
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
