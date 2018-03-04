module Main exposing (main)

import Array
import Canvas
import Graphics2D exposing (point, size)
import Html
import List.Extra
import List.Split
import Matrix exposing (..)
import Model exposing (..)
import Mouse exposing (Button(MainButton))
import NeuralNetwork exposing (Inputs, NeuralNetwork, Targets, query, train)
import Random
import Random.List
import RenderEngine
import View


main: Program Never Model Msg
main = Html.program
    { init = (initialModel, Cmd.batch
        [ Canvas.initCanvas primaryCanvas
        , NeuralNetwork.randomize RandomizeNetwork initialNetwork
        ]
        )
    , view = View.view
    , update = update
    , subscriptions = \model -> Sub.batch
        [ Canvas.onInitCanvas OnInitPrimaryCanvas
        , Canvas.onGetImageData OnGetCanvasImage
        ]
    }



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        None ->
            (model, Cmd.none)

        OnInitPrimaryCanvas canvas ->
            ({model| primaryCanvas = canvas}, Cmd.none)
        OnTargetChange target ->
            ({model| target = target}, Cmd.none)
        OnGetCanvasImage {url, data} ->
            List.Split.chunksOfLeft 4 data
                |> List.map List.sum
                |> List.map (\x -> toFloat x / 255.0 * 0.99 |> (+) 0.01)
                |> (\data -> ({model| imageData = {url = url, data = data }}, Cmd.none))
        StartDraw event ->
            ({model | draw = True}, RenderEngine.render model.primaryCanvas [Graphics2D.moveTo event.offsetPos, Graphics2D.lineWidth 20, Graphics2D.beginPath] )
        Draw event ->
            case model.draw of
                True ->
                    (model, RenderEngine.render model.primaryCanvas [Graphics2D.lineTo event.offsetPos, Graphics2D.strokePath] )
                False ->
                    (model, Cmd.none)
        FinishDraw event ->
            ({model | draw = False}, Canvas.getImageData model.primaryCanvas (point 0 0) (size 28 28))

        RandomizeNetwork network ->
            ({model | network = network}, Cmd.none)

        OnShuffleTrainData data ->
            ({model | trainData = data}, Cmd.none)

        StartTraining ->
            ({model| network = List.foldr train model.network (List.map (\{inputs, targets, name, url} -> (targets,inputs)) model.trainData)}, Cmd.none)

        GuessSymbol ->
            ({ model | guess = toString <| NeuralNetwork.query model.imageData.data model.network}
            , Cmd.batch
                [ RenderEngine.render model.primaryCanvas [Graphics2D.clearRect (point 0 0) model.primaryCanvas.size]
                , Canvas.getImageData model.primaryCanvas (point 0 0) (size 28 28)
                ]
            )

        ShuffleTrainData ->
            (model, Random.List.shuffle model.trainData |> Random.generate OnShuffleTrainData)

        ClickTrainData selected ->
            case List.Extra.find ((==) selected) model.selectionTrainData of
                Just result ->
                    ({model| selectionTrainData = List.Extra.remove result model.selectionTrainData}, Cmd.none)
                Nothing ->
                    ({model| selectionTrainData = selected :: model.selectionTrainData}, Cmd.none)

        RemoveTrainData ->
            List.filter (\e -> not <| List.member e.url model.selectionTrainData) model.trainData
                |> (\t -> {model | trainData = t})
                |> (\m -> ({m | selectionTrainData = []}, Cmd.none))

        AddTrainingData ->
            String.toInt model.target
                |> Result.withDefault 0
                |> target
                |> (\(name,list) ->
                    {inputs = model.imageData.data, targets = list, url = model.imageData.url, name = name}
                        |> flip (::) model.trainData
                   )
                |> (\data -> {model| trainData = data})
                |> (\m ->
                    (m, Cmd.batch
                        [ RenderEngine.render model.primaryCanvas [Graphics2D.clearRect (point 0 0) model.primaryCanvas.size]
                        , Canvas.getImageData model.primaryCanvas (point 0 0) (size 28 28)
                        ]
                    ))
        ClearCanvas ->
             (model, Cmd.batch
                 [ RenderEngine.render model.primaryCanvas [Graphics2D.clearRect (point 0 0) model.primaryCanvas.size]
                 , Canvas.getImageData model.primaryCanvas (point 0 0) (size 28 28)
                 ]
             )


target: Int -> (String, List Float)
target i =
    Array.repeat 10 0.01
        |> Array.set i 0.99
        |> Array.toList
        |> (\l -> (toString i, l))
