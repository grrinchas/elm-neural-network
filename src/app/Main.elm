module Main exposing (main)

import Array
import Canvas
import Graphics2D exposing (point, size)
import Html
import List.Extra
import List.Split
import Matrix exposing (..)
import Model exposing (..)
import NeuralNetwork exposing (Inputs, NeuralNetwork, Targets, query, train)
import Random
import Random.List
import RenderEngine
import View


main: Program Never Model Msg
main = Html.program
    { init = (initialModel, Cmd.batch
        [ Canvas.initCanvas primaryCanvas
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
            ( model, Cmd.none)

        OnInitPrimaryCanvas canvas ->
            ({ model | primaryCanvas = canvas}, Cmd.none)
        OnTargetChange target ->
            ({ model | target = target}, Cmd.none)
        OnLearningRateChange rate ->
            ({ model | learningRate = rate}, Cmd.none)
        OnHiddenChange hidden ->
            ({ model | hidden = hidden}, Cmd.none)
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
            { model | network = network }
                |> (\m -> (trainNetwork m, Cmd.none))

        OnShuffleTrainData data ->
            ({model | trainData = data}, Cmd.none)

        StartTraining ->
            let targets =
                    List.map .name model.trainData
                        |> List.sort
                        |> List.Extra.unique
                        |> Array.fromList
                        |> Array.toIndexedList
                findTargets =
                    \data ->
                        List.map (\(i, name) -> (name, target (List.length targets) i)) targets
                            |> List.Extra.find (\(name, target) -> data.name == name)
                            |> Maybe.withDefault ("", [])
                            |> Tuple.second
                oldNetwork = model.network
                newModel =
                    { model | network =
                        { oldNetwork
                            | learningRate = Maybe.withDefault initialNetwork.learningRate model.learningRate
                            , output = if model.epochs == 0 then List.length targets else oldNetwork.output
                            , hidden = Maybe.withDefault initialNetwork.hidden model.hidden
                        }
                        , trainData = List.map (\data -> {data | targets = findTargets data}) model.trainData
                    }
            in case (model.epochs == 0) of
                   True ->
                       (newModel, NeuralNetwork.randomize RandomizeNetwork newModel.network)
                   False ->
                       (trainNetwork newModel, Cmd.none)

        GuessSymbol ->
            ({ model | guess = Matrix.flatten <| NeuralNetwork.query model.imageData.data model.network}, clearCanvas model)

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

        RemoveAllTrainData ->
            ({model | selectionTrainData = [], trainData = []}, Cmd.none)

        AddTrainingData ->
            {inputs = model.imageData.data, targets = [], url = model.imageData.url, name = model.target}
                |> flip (::) model.trainData
                |> (\data -> ({model | trainData = data}, clearCanvas model))

        ClearCanvas ->
            (model, clearCanvas model )

        ResetNetwork ->
            ({model
                | network = initialNetwork
                , epochs = 0
                , guess = []
                , learningRate = initialModel.learningRate
                , hidden = initialModel.hidden
                }, Cmd.none )


trainNetwork: Model -> Model
trainNetwork model =
    List.map (\data -> (data.targets, data.inputs)) model.trainData
        |> List.foldr train model.network
        |> (\network -> {model | network = network, epochs = model.epochs + 1})


clearCanvas: Model -> Cmd msg
clearCanvas model = Cmd.batch
     [ RenderEngine.render model.primaryCanvas [Graphics2D.clearRect (point 0 0) model.primaryCanvas.size]
     , Canvas.getImageData model.primaryCanvas (point 0 0) (size 28 28)
     ]


target: Int -> Int -> (List Float)
target total i =
    Array.repeat total 0.01
        |> Array.set i 0.99
        |> Array.toList
