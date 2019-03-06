module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)


type alias Model =
    { text : String }

initialModel =
    { text = "" }

type Msg =
    UpdateText String

celsiusToFarenheit c = toFloat(c) * 9 / 5 + 32

inputToOutput s =
  case String.toInt s of
    Nothing ->
      ""
    Just a ->
      a |> celsiusToFarenheit |> String.fromFloat

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateText s ->
      { model | text = inputToOutput s }


view : Model -> Html Msg
view model =
    div []
        [ input
          [ type_ "text"
          , autocomplete False
          , onInput UpdateText
          ]
          []
        , text " Celsius = "
        , input
          [ type_ "text"
          , autocomplete False
          , value model.text
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

