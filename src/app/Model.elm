module Model exposing (..)

import Http
import NeuralNetwork exposing (Inputs, NeuralNetwork, Targets)


type Msg
    = None
    | RandomizeNetwork NeuralNetwork
    | FetchData (Result Http.Error String)
    | StartTraining

type alias Model =
    { network: NeuralNetwork
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


initialModel: Model
initialModel =
    { network = initialNetwork
    , trainData = Nothing
    , testData = Nothing
    , testResults = []
    }
