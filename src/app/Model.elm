module Model exposing (..)

import Canvas exposing (Canvas)
import Graphics2D exposing (size)
import Http
import Mouse exposing (Event)
import NeuralNetwork exposing (Inputs, NeuralNetwork, Targets)


type Msg
    = None
    | OnInitPrimaryCanvas Canvas
    | OnInitSecondaryCanvas Canvas
    | OnGetCanvasImage (List Int)
    | GetCanvasImage
    | StartDraw Event
    | Draw Event
    | FinishDraw Event

    | RandomizeNetwork NeuralNetwork
    | StartTraining


type alias Model =
    { primaryCanvas: Canvas
    , secondaryCanvas: Canvas
    , network: NeuralNetwork
    , canvasImages: List (List Int)
    , draw: Bool
    , trainData: Maybe (List (Targets, Inputs))
    , testData: Maybe (List (Targets, Inputs))
    , testResults: List String
    }


initialNetwork: NeuralNetwork
initialNetwork =
    { input = 784
    , output = 10
    , hidden = 200
    , learningRate = 0.4
    , wih = []
    , woh = []
    }


primaryCanvas: Canvas
primaryCanvas =
    { id = "primaryCanvas"
    , size = size 400 400
    , assets = []
    }

secondaryCanvas: Canvas
secondaryCanvas =
    { id = "secondaryCanvas"
    , size = size 28 28
    , assets = []
    }

initialModel: Model
initialModel =
    { primaryCanvas = primaryCanvas
    , secondaryCanvas = secondaryCanvas
    , canvasImages = []
    , draw = False
    , network = initialNetwork
    , trainData = Nothing
    , testData = Nothing
    , testResults = []
    }

