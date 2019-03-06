module Main exposing (main)

import Browser
import Html exposing (Html, div, text, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)


type alias Model =
    { c : String, f : String }

initialModel =
    { c = "", f = ""}

type Msg =
    CelsiusUpdated String
      | FarenheitUpdated String

celsiusToFarenheit c = c * 9 / 5 + 32
farenheitToCelsius f = (f - 32) * (5/9)

inputToOutput converter s =
  case String.toFloat s of
    Nothing ->
      ""
    Just a ->
      a |> converter |> String.fromFloat

update : Msg -> Model -> Model
update msg model =
  case msg of
    CelsiusUpdated s ->
      { model | c = s, f = inputToOutput celsiusToFarenheit s }
    FarenheitUpdated s ->
      { model | f = s, c = inputToOutput farenheitToCelsius s }

view : Model -> Html Msg
view model =
    div []
        [ input
          [ type_ "text"
          , autocomplete False
          , value model.c
          , onInput CelsiusUpdated
          ]
          []
        , text " Celsius = "
        , input
          [ type_ "text"
          , autocomplete False
          , value model.f
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

