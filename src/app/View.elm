module View exposing (..)

import Color exposing (Color)
import Color.Manipulate
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input exposing (autocomplete, choice, disabled, dropMenu, hiddenLabel, menu, placeholder, select, styledChoice)
import Html exposing (Html)
import Html.Attributes
import Model exposing (..)
import Mouse
import Style exposing (..)
import Style.Color exposing (background, border)
import Style.Border as Border
import Style.Font as Font
import Style.Shadow as Shadow


primaryColor: Color
primaryColor = Color.rgba 29 53 87 1

secondaryColor: Color
secondaryColor = Color.rgba 230 57 70 1

whiteColor: Color
whiteColor = Color.rgba 241 250 238 1

robotoFont: Font
robotoFont = Font.importUrl {url = "https://fonts.googleapis.com/css?family=Roboto:300,400", name = "Roboto"}

containerWidth: Float
containerWidth = 1000

shadow: Property class variation
shadow = Shadow.box
    { offset = (0, 0)
    , size = 1
    , blur = 2
    , color = Color.rgba 0 0 0 0.30
    }



type Styles
    = NoneStyle
    | CanvasStyle
    | SelectedTrainData
    | HeaderStyle
    | NeuralNetworkHeadline
    | ControlPanel
    | ButtonStyle
    | CanvasPanel
    | DisabledButton
    | StartButton
    | TargetInputStyle
    | Instruction
    | H2
    | GuessStyle


stylesheet: StyleSheet Styles var
stylesheet =
    Style.styleSheet
        [ style NoneStyle []
        , style CanvasStyle
            [ Border.all 1
            , border <| Color.rgba 0 0 0 0.1
            , background Color.white
            ]
        , style SelectedTrainData
            [ Border.all 1
            , border <| Color.red
            , background Color.white
            ]
        , style HeaderStyle
            [ background primaryColor
            , Style.Color.text whiteColor
            , Font.size 35
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.light
            ]
        , style GuessStyle
            [ Style.Color.text <| Color.rgba 0 0 0 0.87
            , Font.size 35
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.light
            ]
        , style NeuralNetworkHeadline
            [ Style.Color.text secondaryColor
            , Font.size 35
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.weight 400
            ]
        , style ControlPanel
            [ background <| Color.Manipulate.lighten 0.6 primaryColor
            ]
        , style CanvasPanel
            [ background <| Color.Manipulate.lighten 0.95 Color.black
            ]
        , style TargetInputStyle
            [ Style.Color.text <| Color.rgba 0 0 0 0.87
            , Font.size 24
            , Font.bold
            ]
        , style ButtonStyle
            [ background <| primaryColor
            , Style.Color.text <| whiteColor
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , Font.size 16
            , hover [
                background <| Color.Manipulate.lighten 0.05 primaryColor
                ]
            ]
        , style StartButton
            [ background <| Color.Manipulate.lighten 0 secondaryColor
            , Style.Color.text <| whiteColor
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , Font.size 16
            , hover [
                background <| Color.Manipulate.lighten 0.05 secondaryColor
                ]
            ]
        , style DisabledButton
            [ background <| Color.Manipulate.lighten 0.5 primaryColor
            , Style.Color.text <| Color.rgba 255 255 255 0.6
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , Font.size 16
            , Font.center
            , hover
                [ cursor "default !important"
                ]
            ]
        , style Instruction
            [ Font.size 18
            ]
        , style H2
            [ Font.bold
            , Font.size 24
            ]
        ]


view: Model -> Html Msg
view model = layout stylesheet <|
    column NoneStyle []
        [ column HeaderStyle [spacingXY 0 10, width fill, paddingXY 0 100, center, verticalCenter]
            [ row NoneStyle []
                [ text "Create and train your own "
                , el NeuralNetworkHeadline [] (text "Neural Network")
                , text " to recognise "
                ]
            ,
            row NoneStyle []
                [ text "any symbol."
                ]
            ]
        , column ControlPanel [width fill, center, paddingXY 0 20]
            [ row NoneStyle [width <| px containerWidth, spacing 20]
                [ row NoneStyle [spacing 10]
                    [ button ButtonStyle [width <| px 100, paddingXY 10 7, Mouse.onClick <| always AddTrainingData] (text "Add")
                    , case List.isEmpty model.selectionTrainData  of
                        True -> el DisabledButton [width <| px 100, paddingXY 10 7] (text "Remove")
                        False -> button ButtonStyle [width <| px 100, paddingXY 10 7, Mouse.onClick <| always RemoveTrainData] (text "Remove")
                    , case List.isEmpty model.trainData  of
                        True ->  el DisabledButton [width <| px 100, paddingXY 10 7] (text "Shuffle")
                        False -> button ButtonStyle [width <| px 100, paddingXY 10 7, Mouse.onClick <| always ShuffleTrainData] (text "Shuffle")
                    ,case List.isEmpty model.trainData  of
                        True -> el DisabledButton [width <| px 100, paddingXY 10 7 ] (text "Train")
                        False -> button StartButton [width <| px 100, paddingXY 10 7, Mouse.onClick <| always StartTraining] (text "Train")
                    , case List.isEmpty model.trainData  of
                        True -> el DisabledButton [width <| px 100, paddingXY 10 7 ] (text "Guess")
                        False -> button ButtonStyle [width <| px 100, paddingXY 10 7, Mouse.onClick <| always GuessSymbol] (text "Guess")
                    , button ButtonStyle [width <| px 100, paddingXY 10 7, Mouse.onClick <| always ClearCanvas] (text "Clear")
                    ]
                ]
            ]
        , column CanvasPanel [spacing 20 , width fill, center, paddingXY 0 20 ]
            [ row NoneStyle [width <| px containerWidth, spacingXY 20 0 ]
                [ column NoneStyle []
                    [ h2 H2 [paddingBottom 10] (text "Training data")
                    , wrappedRow NoneStyle
                        [ width <| px 290, spacing 5
                        , alignTop, yScrollbar
                        , maxHeight <| px model.primaryCanvas.size.height
                        , minHeight <| px model.primaryCanvas.size.height
                        ] <|
                         List.map (\{inputs, targets, url, name} ->
                            image (if List.member url model.selectionTrainData then SelectedTrainData else CanvasStyle)
                                [ width <| px 50, height <| px 50
                                , Mouse.onClick <| always <| ClickTrainData url
                                ] {src = url, caption = ""}
                                ) model.trainData
                    ]

                , column NoneStyle []
                    [ row NoneStyle [paddingBottom 10 ]
                        [ Element.Input.text TargetInputStyle [height <| px 30]
                            { onChange = OnTargetChange
                            , value = ""
                            , label = placeholder {text = "Name", label = hiddenLabel ""}
                            , options = []
                            }
                        ]

                    , el CanvasStyle
                        [ Mouse.onDown StartDraw
                        , Mouse.onMove Draw
                        , Mouse.onUp FinishDraw
                        ] <| html <|
                        Html.canvas
                            [ Html.Attributes.id model.primaryCanvas.id
                            , Html.Attributes.width <| round model.primaryCanvas.size.width
                            , Html.Attributes.height <| round model.primaryCanvas.size.height
                            ] []

                    ]
                , column NoneStyle [spacingXY 0 5]
                    [ h2 H2 [paddingBottom 10] (text "Instructions")
                    , el Instruction [] (text "Step 1: Draw a symbol on the canvas.")
                    ,el Instruction [] (text "Step 2: Give it a name.")
                    ,el Instruction [] (text "Step 3: Add it to the train data.")
                    ,el Instruction [] (text "Step 4: Repeat.")
                    ,el Instruction [] (text "Step 5: Train neural network.")
                    ,el Instruction [] (text "Step 6: Test with new symbol.")
                    ]
                ]
            ]

        , column NoneStyle [height <| px 200, center]
            [ row NoneStyle [width <| px containerWidth, paddingXY 0 50]
                [ el GuessStyle [] (text <| "My best guess: " ++ (toString  model.guess))
                ]
            ]
        ]


toNumber: List Float -> Int
toNumber list =
    List.indexedMap (,) list
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Maybe.withDefault (-1, 0.0)
        |> Tuple.first




