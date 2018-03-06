module Model exposing (..)

import Canvas exposing (Canvas)
import Element.Input exposing (SelectMsg)
import Graphics2D exposing (size)
import Http
import Mouse exposing (Event)
import NeuralNetwork exposing (Inputs, NeuralNetwork, Targets)


type alias Symbol = String

type alias TrainData =
    { inputs: List Float
    , targets: List Float
    , url: String
    , name: String
    }

type Msg
    = None
    | OnInitPrimaryCanvas Canvas
    | OnGetCanvasImage {url: String, data: List Int}
    | OnTargetChange String
    | OnLearningRateChange Float
    | OnHiddenChange Int
    | StartDraw Event
    | Draw Event
    | FinishDraw Event

    | RandomizeNetwork NeuralNetwork
    | StartTraining
    | GuessSymbol
    | AddTrainingData
    | OnShuffleTrainData (List TrainData)
    | ShuffleTrainData
    | ClickTrainData String
    | RemoveTrainData
    | RemoveAllTrainData
    | ClearCanvas
    | ResetNetwork



initialNetwork: NeuralNetwork
initialNetwork =
    { input = 784
    , output = 0
    , hidden = 200
    , learningRate = 0.2
    , wih = []
    , woh = []
    }


primaryCanvas: Canvas
primaryCanvas =
    { id = "primaryCanvas"
    , size = size 400 400
    , assets = []
    }


type alias Model =
    { primaryCanvas: Canvas
    , network: NeuralNetwork
    , draw: Bool
    , target: String
    , trainData: List TrainData
    , imageData: {url:String, data: List Float}
    , guess: List Float
    , selectionTrainData: List String
    , epochs: Int
    , trainingStatus: Bool
    }

initialModel: Model
initialModel =
    { primaryCanvas = primaryCanvas
    , draw = False
    , network = initialNetwork
    , trainData = []
    , imageData = {url = "", data = List.repeat 784 0.01}
    , target = ""
    , guess = []
    , selectionTrainData = []
    , epochs = 0
    , trainingStatus = False
    }

