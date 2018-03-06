module View exposing (..)

import Array
import Color exposing (Color)
import Color.Manipulate
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input exposing (autocomplete, choice, disabled, dropMenu, hiddenLabel, menu, placeholder, select, styledChoice)
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Model exposing (..)
import Mouse
import Round
import Style exposing (..)
import Style.Color exposing (background, border)
import Style.Border as Border
import Style.Font as Font
import Style.Shadow as Shadow


robotoFont: Font
robotoFont = Font.importUrl {url = "https://fonts.googleapis.com/css?family=Roboto:300,400", name = "Roboto"}

containerWidth: Float
containerWidth = 1000


type ColorPalette
    = PrimaryColor
    | SecondaryColor
    | WhiteColor
    | BlackColor


color: ColorPalette -> Color
color c =
    case c of
        PrimaryColor -> Color.rgba 29 53 87 1
        SecondaryColor -> Color.rgba 230 57 70 1
        WhiteColor -> Color.white
        BlackColor -> Color.rgba 0 0 0 0.87


bgColor: Styles -> Property class variation
bgColor s =
    case s of
        CanvasStyle ->
            background <| color WhiteColor
        SelectedTrainData ->
            background <| color WhiteColor
        ControlPanel ->
            background <| Color.Manipulate.lighten 0.6 (color PrimaryColor)
        CanvasPanel ->
            background <| Color.Manipulate.lighten 0.94 (color BlackColor)
        Button DisabledButton ->
            background <| Color.Manipulate.lighten 0.5 (color PrimaryColor)
        Button TrainButton ->
            background <| color SecondaryColor
        Button NormalButton ->
            background <| color PrimaryColor
        HeaderStyle ->
            background <| color PrimaryColor
        _ ->
            background <| color PrimaryColor


fgColor: Styles -> Property class variation
fgColor s =
    case s of
        LabelStyle ->
            Style.Color.text <| Color.Manipulate.lighten 0.5 (color BlackColor)
        TargetInputStyle ->
            Style.Color.text <| color BlackColor
        NeuralNetworkHeadline ->
            Style.Color.text <| color SecondaryColor
        Link ->
            Style.Color.text <| Color.Manipulate.lighten 0.3 (color PrimaryColor)
        Button NormalButton ->
            Style.Color.text <| color WhiteColor
        Button DisabledButton ->
            Style.Color.text <| color WhiteColor
        Button TrainButton ->
            Style.Color.text <| color WhiteColor
        HeaderStyle ->
            Style.Color.text <| color WhiteColor
        _ ->
            Style.Color.text <| color BlackColor


borderColor: Styles -> Property class variation
borderColor s =
    case s of
        SelectedTrainData ->
            Style.Color.border <| color SecondaryColor
        CanvasStyle ->
            Style.Color.border <| Color.Manipulate.lighten 0.8 (color BlackColor)

        _ ->
            Style.Color.border <| color PrimaryColor

fontSize: Styles -> Property class variation
fontSize s =
    case s of
        HeaderStyle -> Font.size 35
        NeuralNetworkHeadline -> Font.size 35
        H1 -> Font.size 35
        LabelStyle -> Font.size 18
        Paragraph -> Font.size 18
        Button _ -> Font.size 16
        TextStyle -> Font.size 18
        _ -> Font.size 18

type ButtonStyle
    = NormalButton
    | DisabledButton
    | TrainButton

type Styles
    = NoneStyle
    | CanvasStyle
    | SelectedTrainData
    | HeaderStyle
    | NeuralNetworkHeadline
    | ControlPanel
    | CanvasPanel
    | TargetInputStyle
    | H1
    | Paragraph
    | Link
    | Button ButtonStyle
    | LabelStyle
    | TextStyle

stylesheet: StyleSheet Styles var
stylesheet =
    Style.styleSheet
        [ style NoneStyle []
        , style CanvasStyle
            [ Border.all 1
            , borderColor CanvasStyle
            , bgColor CanvasStyle
            ]
        , style SelectedTrainData
            [ Border.all 1
            , bgColor SelectedTrainData
            , borderColor SelectedTrainData
            ]

        , style HeaderStyle
            [ bgColor HeaderStyle
            , fgColor HeaderStyle
            , fontSize HeaderStyle
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.light
            ]
        , style NeuralNetworkHeadline
            [ fgColor NeuralNetworkHeadline
            ,fontSize NeuralNetworkHeadline
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.weight 400
            ]
        , style ControlPanel
            [ bgColor ControlPanel
            ]
        , style CanvasPanel
            [ bgColor CanvasPanel
            ]
        , style TargetInputStyle
            [ fgColor TargetInputStyle
            , prop "font-size" "18px !important"
            , prop "border-bottom" "1px dashed rgba(0, 0, 0, 0.4) !important"
            , pseudo "focus"
                [ prop "box-shadow" "none !important"
                ]
            ]
        , style (Button NormalButton)
            [ bgColor <| Button NormalButton
            , fgColor <| Button NormalButton
            , fontSize <| Button NormalButton
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , hover [
                background <| Color.Manipulate.lighten 0.05 (color PrimaryColor)
                ]
            ]
        , style (Button TrainButton)
            [ bgColor <| Button TrainButton
            , fgColor <| Button TrainButton
            , fontSize <| Button TrainButton
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , hover [
                background <| Color.Manipulate.lighten 0.05 (color SecondaryColor)
                ]
            ]
        , style (Button DisabledButton)
            [ bgColor <| Button DisabledButton
            , fgColor <| Button DisabledButton
            , fontSize <| Button DisabledButton
            , Font.uppercase
            , Border.rounded 3
            , Font.letterSpacing 1
            , Font.center
            , hover
                [ cursor "default !important"
                ]
            ]
        , style H1
            [ fontSize H1
            , Font.typeface [robotoFont, Font.sansSerif]
            , Font.light
            ]
        , style Paragraph
            [ fontSize Paragraph
            , Font.lineHeight 1.47
            ]
        , style Link
            [ fgColor Link
            ]
        , style TextStyle
            [ fontSize TextStyle
            , fgColor TextStyle
            ]
        , style LabelStyle
            [ fgColor LabelStyle
            , Font.bold
            , fontSize LabelStyle
            ]
        ]




view: Model -> Html Msg
view model = layout stylesheet <|
    column NoneStyle [minWidth <| px containerWidth]
        [ column HeaderStyle [spacingXY 0 10, width fill, paddingXY 0 100, center, verticalCenter]
            [ row NoneStyle []
                [ text "Create and train your own "
                , el NeuralNetworkHeadline [] (text "Neural Network")
                , text " to recognise "
                ]
            ,
            row NoneStyle []
                [ text "any pattern."
                ]
            ]

        , column ControlPanel [width fill, center, paddingXY 0 20]
            [ row NoneStyle [width <| px containerWidth, spread]
                [ row NoneStyle [spacing 10]
                    [ case List.isEmpty model.trainData  of
                        True ->
                            el (Button DisabledButton) [width <| px 100, paddingXY 10 7 ] (text ("Train (" ++ toString model.epochs ++ ")"))
                        False ->
                            button (Button TrainButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always StartTraining] (text ("Train (" ++ toString model.epochs ++ ")"))
                    , case model.epochs == 0 of
                        True ->
                            el (Button DisabledButton) [width <| px 100, paddingXY 10 7 ] (text "Guess")
                        False -> button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always GuessSymbol] (text "Guess")
                    , case model.epochs == 0 of
                        True -> el (Button DisabledButton) [width <| px 100, paddingXY 10 7 ] (text "Reset")
                        False -> button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always ResetNetwork] (text "Reset")
                    ]

                , row NoneStyle [spacing 10]
                    [ button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always ClearCanvas] (text "Clear")
                    , case List.length model.trainData < 2 of
                        True ->  el (Button DisabledButton) [width <| px 100, paddingXY 10 7] (text "Shuffle")
                        False -> button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always ShuffleTrainData] (text "Shuffle")
                    , case model.epochs > 0 || (String.trim model.target |> String.isEmpty) of
                        True -> el (Button DisabledButton) [width <| px 100, paddingXY 10 7] (text "Add")
                        False -> button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always AddTrainingData] (text "Add")

                    , case List.isEmpty model.selectionTrainData || model.epochs > 0 of
                        True -> el (Button DisabledButton) [width <| px 100, paddingXY 10 7] (text "Remove")
                        False -> button (Button NormalButton) [width <| px 100, paddingXY 10 7, Mouse.onClick <| always RemoveTrainData] (text "Remove")
                    , case List.isEmpty model.trainData  || model.epochs > 0 of
                        True -> el (Button DisabledButton) [width <| px 135, paddingXY 10 7] (text "Remove All")
                        False -> button (Button NormalButton) [width <| px 135, paddingXY 10 7, Mouse.onClick <| always RemoveAllTrainData] (text "Remove All")
                    ]
                ]
            ]
        , column CanvasPanel [spacing 20 , width fill, center, paddingXY 0 20 ]
            [ row NoneStyle [width <| px containerWidth, spacingXY 20 0 ]
                [ column NoneStyle [width <| px model.primaryCanvas.size.width]
                    [ row NoneStyle [paddingBottom 20]
                        [ Element.Input.text TargetInputStyle [height <| px 20, width <| px 40,attribute "maxlength" "4"]
                            { onChange = always None
                            , value = toString model.network.hidden
                            , label = Element.Input.labelLeft <|  h2 LabelStyle [] <| text "Hidden: "
                            , options = []
                            }
                        , Element.Input.text TargetInputStyle [height <| px 20, width <| px 37,attribute "maxlength" "4", moveLeft 70]
                            { onChange = always None
                            , value = toString model.network.learningRate
                            , label = Element.Input.labelLeft <|  h2 LabelStyle [moveLeft 70] <| text "LR: "
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
                   , row NoneStyle [paddingTop 20]
                    [ h2 LabelStyle [] (text "Results: ")
                    , wrappedRow NoneStyle [width <| px containerWidth, alignLeft] <|
                       List.map (\(name, guess) -> el NoneStyle [alignLeft] <| text (name ++ ": " ++ guess ++ "%, ")) (toNumber model)
                    ]
                ]
                , column NoneStyle []
                    [ row NoneStyle []
                        [ Element.Input.text TargetInputStyle [height <| px 20, width <| px 90, attribute "maxlength" "10"]
                            { onChange = OnTargetChange
                            , value = ""
                            , label = Element.Input.labelLeft <| h2 LabelStyle [] <| text "Name: "
                            , options = []
                            }
                        , h2 LabelStyle [paddingBottom 20, moveLeft 260] (text ("Training data: "))
                        , el TextStyle [moveLeft 260] <| text <| toString (List.length model.trainData)

                        ]
                    , wrappedRow NoneStyle
                        [ spacing 5
                        , paddingRight 5
                        , alignTop

                        , yScrollbar
                        , maxHeight <| px model.primaryCanvas.size.height
                        , height <| px model.primaryCanvas.size.height
                        , width <| px 570
                        ] <|
                         List.map (\{inputs, targets, url, name} ->
                            column NoneStyle []
                                [ image (if List.member url model.selectionTrainData then SelectedTrainData else CanvasStyle)
                                    [ width <| px 50, height <| px 50
                                    , Mouse.onClick <| always <| ClickTrainData url
                                    ] {src = url, caption = ""}
                                ]
                                ) model.trainData

                    ]
                ]
            ]
           , column NoneStyle [width fill, center]
               [ column NoneStyle [width <| px 800]
                   [ h1  H1 [paddingTop 60, paddingBottom 20] (text "What is it?")
                   , paragraph Paragraph []
                       [ text <| "It is an experimental application to test pattern recognition with neural network written entirely in "
                       , link "http://elm-lang.org/" <| el Link [] (text "Elm")
                       , text <| " programming language. "
                       , text <| "It allows to create your own training set for various patterns, train neural network "
                       , text <| "to recognise them, and lastly test it."
                       ]
                   , h1  H1 [paddingTop 60, paddingBottom 20] (text "How to use it?")
                   , paragraph Paragraph []
                        [ text <| ""
                        ]
                   , h1  H1 [paddingTop 60, paddingBottom 20] (text "How does it work?")
                   , text ("")
                   ]
               ]
        ]




toNumber: Model -> List (String, String)
toNumber model =
    List.map .name model.trainData
        |> List.sort
        |> List.Extra.unique
        |> List.map2 (\guess name -> (name, Round.round 2 <| guess * 100)) model.guess
        |> List.sortBy (Tuple.second)
        |> List.reverse





