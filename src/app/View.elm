module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)



view: Model -> Html Msg
view model =
    case model.trainData of
        Nothing ->
            div []
                [ button [onClick StartTraining, disabled True, style [("opacity", "0.5")] ] [text "Start Training"]
                , p [] [text "loading data"]
                ]
        Just data ->
            div []
                [ button [onClick StartTraining, disabled False, style [("opacity", "1")] ] [text "Start Training"]
                , p [] [text "data is ready"]
                ]

