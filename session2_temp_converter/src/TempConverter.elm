module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { c : Maybe Float, f : Maybe Float }


initialModel =
    { c = Nothing, f = Nothing }


type Msg
    = CelsiusUpdated String
    | FarenheitUpdated String


celsiusToFarenheit c =
    c * 9 / 5 + 32


farenheitToCelsius f =
    (f - 32) * (5 / 9)


convertInputData converter s =
    case String.toFloat s of
        Nothing ->
            Nothing

        Just a ->
            Just (converter a)


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusUpdated s ->
            { model | c = String.toFloat s, f = convertInputData celsiusToFarenheit s }

        FarenheitUpdated s ->
            { model | f = String.toFloat s, c = convertInputData farenheitToCelsius s }


decorateModel model =
    let
        decorateMaybeFloat x =
            case x of
                Nothing ->
                    ""

                Just y ->
                    String.fromFloat y
    in
    { c = decorateMaybeFloat model.c, f = decorateMaybeFloat model.f }


view : Model -> Html Msg
view model =
    let
        decoratedModel =
            decorateModel model
    in
    div []
        [ input
            [ type_ "text"
            , autocomplete False
            , value decoratedModel.c
            , onInput CelsiusUpdated
            ]
            []
        , text " Celsius = "
        , input
            [ type_ "text"
            , autocomplete False
            , value decoratedModel.f
            , onInput FarenheitUpdated
            ]
            []
        , text " Farenheit"
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
