module View exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import Html.Attributes
import Model exposing (..)
import Mouse
import Style exposing (..)
import Style.Color exposing (background, border)
import Style.Border as Border


type Styles
    = NoneStyle
    | CanvasStyle


stylesheet: StyleSheet Styles var
stylesheet =
    Style.styleSheet
        [ style NoneStyle []
        , style CanvasStyle
            [ Border.all 1
            , border <| Color.rgba 0 0 0 0.3
            ]
        ]

view: Model -> Html Msg
view model = layout stylesheet <|
    row NoneStyle [spacing 10]
        [ column NoneStyle [spacing 10]
            [ el CanvasStyle
                [ Mouse.onDown StartDraw
                , Mouse.onMove Draw
                , Mouse.onUp FinishDraw
                ] <| html <|
                Html.canvas
                    [ Html.Attributes.id model.primaryCanvas.id
                    , Html.Attributes.width <| round model.primaryCanvas.size.width
                    , Html.Attributes.height <| round model.primaryCanvas.size.height
                    ] []
            , button NoneStyle [width <| px 100, Mouse.onClick <| always GetCanvasImage] (text "Create Image")
            ]
        , column NoneStyle [spacing 10]
            [ el CanvasStyle [] <| html <|
                Html.canvas
                    [ Html.Attributes.id model.secondaryCanvas.id
                    , Html.Attributes.width <| round model.secondaryCanvas.size.width
                    , Html.Attributes.height <| round model.secondaryCanvas.size.height
                    ] []
            , text <| toString model.canvasImages
            ]
        ]








{-

view: Model -> Html Msg
view model =
    div []
    [ canvas
        [ id model.canvas.id
        , height model.canvas.height
        , width model.canvas.width
        ] []
    , case model.trainData of
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
    ]
-}
