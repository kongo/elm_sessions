-- CRUD http://eugenkiss.github.io/7guis/tasks/
import Browser
import Html exposing (Html, button, div, text, input, option, select)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)

main =
  Browser.sandbox { init = initialModel, update = update, view = view }

type alias Profile = { name : String, surname : String }
emptyProfile = Profile "" ""

type alias Model = { newProfile : Profile, profiles : List Profile, filter : String }
initialModel = Model emptyProfile [] ""

type Msg
    = Create
    | NameUpdated String
    | SurnameUpdated String
    | FilterChanged String

update msg model =
  let
      profileWithName profile name = { profile | name = name }
      profileWithSurname profile surname = { profile | surname = surname }
  in
      case msg of
          Create ->
              { model | newProfile = emptyProfile , profiles = model.profiles ++ [ model.newProfile ] }
          NameUpdated s ->
              { model | newProfile = profileWithName model.newProfile s }
          SurnameUpdated s ->
              { model | newProfile = profileWithSurname model.newProfile s }
          FilterChanged s ->
              { model | filter = s }

view model =
    let
        profileToOption profile = 
            option [] [ text (String.join ", " [ profile.surname, profile.name ]) ]

        filteredProfiles =
            if model.filter == "" then
                model.profiles
            else
                List.filter (\profile -> String.startsWith model.filter profile.surname) model.profiles
    in
        div [ id "elm-node" ]
            [ div [ class "row" ]
                [ div [ class "col-2" ]
                    [ text "Filter prefix:" ]
                , div [ class "col-3" ]
                    [ input [ type_ "text", onInput FilterChanged ]
                        []
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-3" ]
                    [ Html.select [ attribute "multiple" "multiple" ]
                        (List.map profileToOption filteredProfiles)
                    ]
                , div [ class "col-3" ]
                    [ div [ class "row" ]
                        [ div [ class "col-3" ]
                            [ text "Name:" ]
                        , div [ class "col-3" ]
                            [ input [ type_ "text", value model.newProfile.name, onInput NameUpdated ]
                                []
                            ]
                        ]
                    , div [ class "row" ]
                        [ div [ class "col-3" ]
                            [ text "Surname:" ]
                        , div [ class "col-3" ]
                            [ input [ type_ "text", value model.newProfile.surname, onInput SurnameUpdated ]
                                []
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-1" ]
                    [ input [ type_ "button", value "Create", onClick Create ]
                        []
                    ]
                , div [ class "col-1" ]
                    [ input [ type_ "button", value "Update" ]
                        []
                    ]
                , div [ class "col-1" ]
                    [ input [ type_ "button", value "Delete" ]
                        []
                    ]
                ]
            ]
