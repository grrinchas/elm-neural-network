module Main exposing (main)

import Array
import Html exposing (Html, div, h1, p, text)
import Http
import Matrix exposing (..)
import Random

type Msg
    = None
    | GenerateWeights (Matrix Float, Matrix Float)
    | FetchData (Result Http.Error String)



type alias NeuralNetwork =
    { input: Int
    , output: Int
    , hidden: Int
    , learningRate: Float
    , wih: Matrix Float
    , woh: Matrix Float
    }

type alias Model =
    { network: NeuralNetwork
    , trainData: Maybe (List (List Float, List Float))
    }

fetch: Cmd Msg
fetch =
    Http.send FetchData <| Http.getString
    "https://raw.githubusercontent.com/makeyourownneuralnetwork/makeyourownneuralnetwork/master/mnist_dataset/mnist_test_10.csv"



sigmoid: Float -> Float
sigmoid x = 1 / (1 + e^(-x))


hiddenInputs: List Float -> NeuralNetwork -> Matrix Float
hiddenInputs inp network =
        Matrix.dot network.wih (Matrix.colMatrix inp)


hiddenOutputs: List Float -> NeuralNetwork -> Matrix Float
hiddenOutputs inp network =
    hiddenInputs inp network |> Matrix.map sigmoid


finalInputs: List Float -> NeuralNetwork -> Matrix Float
finalInputs inp network =
    hiddenOutputs inp network |> Matrix.dot network.woh


finalOutputs: List Float -> NeuralNetwork -> Matrix Float
finalOutputs inp network =
    finalInputs inp network |> Matrix.map sigmoid


train: List Float -> List Float -> NeuralNetwork -> NeuralNetwork
train inp targets network =
    { input = network.input
    , output = network.output
    , hidden = network.hidden
    , learningRate = network.learningRate
    , woh =
        let outputs = finalOutputs inp network in
        Matrix.map (\x -> 1 - x) outputs
            |> Matrix.multiply outputs
            |> Matrix.multiply (Matrix.subtract (Matrix.colMatrix targets) outputs)
            |> (\x -> hiddenOutputs inp network |> Matrix.transpose |> Matrix.dot x)
            |> Matrix.map ((*) network.learningRate)
            |> Matrix.add network.woh
    , wih =
        let outputs = hiddenOutputs inp network in
        Matrix.map (\x -> 1 - x) outputs
            |> Matrix.multiply outputs
            |> Matrix.multiply (Matrix.dot (Matrix.transpose network.woh) (Matrix.subtract (Matrix.colMatrix targets) (finalOutputs inp network)))
            |> (\x -> Matrix.colMatrix inp |> Matrix.transpose |> Matrix.dot x)
            |> Matrix.map ((*) network.learningRate)
            |> Matrix.add network.wih
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
    }

gaussian: Float -> Float -> Float -> Float
gaussian mean deviation x =
    (e^(-((x - mean)^2)/(2 * deviation ^ 2)))/(sqrt (2 * pi * deviation ^ 2))


main: Program Never Model Msg
main = Html.program
    { init =
        ( initialModel
        , Cmd.batch [Random.float -0.5 0.5
            |> Random.list (initialModel.network.input * initialModel.network.hidden + initialModel.network.hidden * initialModel.network.output)
            |> Random.map (\l -> (
                Matrix.fromList initialModel.network.input <|
                List.take (initialModel.network.input * initialModel.network.hidden) l
                , Matrix.fromList initialModel.network.hidden <| List.drop (initialModel.network.input * initialModel.network.hidden) l))
            |> Random.generate GenerateWeights
            , fetch
            ]
        )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        None -> (model, Cmd.none)
        FetchData data ->
            case data of
                Err err -> (model, Cmd.none)
                Ok result ->
                    let train = List.map trainData <| List.filter (not << String.isEmpty) <| String.lines result in
            ({model| trainData = Just train }, Cmd.none)

        GenerateWeights (wih, woh) ->
            case model.network of
                network ->
                    ({model | network = {network | wih = wih, woh = woh}}, Cmd.none)



view: Model -> Html Msg
view model =
    div []
        [ text <| toString <| dot [[1,2,3],[4,5,6]] [[7,8],[9,10],[11,12]]
         , case model.trainData of
             Nothing ->
                 p [] [text "loading data"]
             Just data ->
                 div []
                    [ p [] [text <| toString <|toNumber <| flatten <| finalOutputs (Tuple.second <| trainData two) (trainMany data model.network)]
                    , p [] [text <| toString <|toNumber <| flatten <| finalOutputs (Tuple.second <| trainData seven) (trainMany data model.network)]
                    , p [] [text <| toString <|toNumber <| flatten <| finalOutputs (Tuple.second <| trainData five) (trainMany data model.network)]
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





trainMany: List (List Float, List Float) -> NeuralNetwork ->  NeuralNetwork
trainMany data network =
   -- train (Tuple.second <| trainData two) (Tuple.first <| trainData two) network
    List.foldr (\(x,y) net -> train y x net) network data
        --|> (\n -> List.foldr (\(x,y) net -> train y x net) n data)

target: Int -> List Float
target i =
    Array.repeat 10 0.01
        |> Array.set i 0.99
        |> Array.toList

seven: String
seven = "7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,84,185,159,151,60,36,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,222,254,254,254,254,241,198,198,198,198,198,198,198,198,170,52,0,0,0,0,0,0,0,0,0,0,0,0,67,114,72,114,163,227,254,225,254,254,254,250,229,254,254,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,66,14,67,67,67,59,21,236,254,106,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,83,253,209,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22,233,255,83,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,129,254,238,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,59,249,254,62,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,133,254,187,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,205,248,58,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,126,254,182,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,75,251,240,57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,221,254,166,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,203,254,219,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,38,254,254,77,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,224,254,115,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,133,254,254,52,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,242,254,254,52,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,121,254,254,219,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,121,254,207,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"

five: String
five = "5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,47,47,47,16,129,85,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,75,153,217,253,253,253,215,246,253,253,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,35,142,244,252,253,253,253,253,253,253,253,253,253,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63,253,253,253,253,253,253,253,213,170,170,170,170,0,0,0,0,0,0,0,0,0,0,0,20,132,72,0,57,238,227,238,168,124,69,20,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,206,253,78,0,0,32,0,30,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,177,253,132,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,133,253,233,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,92,253,223,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,150,253,174,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,234,253,246,127,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,253,253,253,251,147,91,121,85,42,42,85,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,139,253,253,253,253,253,253,253,253,253,253,253,232,168,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,53,218,222,251,253,253,253,253,253,253,253,253,252,124,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,67,72,200,253,253,253,253,253,253,253,175,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,120,253,249,152,51,164,253,253,175,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50,253,253,253,188,252,253,253,148,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,167,253,253,253,253,250,175,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,23,180,231,253,221,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,149,22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"

two: String
two = "2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,116,125,171,255,255,150,93,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,169,253,253,253,253,253,253,218,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,169,253,253,253,213,142,176,253,253,122,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,52,250,253,210,32,12,0,6,206,253,140,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,77,251,210,25,0,0,0,122,248,253,65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31,18,0,0,0,0,209,253,253,65,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,117,247,253,198,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,76,247,253,231,63,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,253,253,144,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,176,246,253,159,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,25,234,253,233,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,198,253,253,141,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,78,248,253,189,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19,200,253,253,141,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,134,253,253,173,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,248,253,253,25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,248,253,253,43,20,20,20,20,5,0,5,20,20,37,150,150,150,147,10,0,0,0,0,0,0,0,0,0,248,253,253,253,253,253,253,253,168,143,166,253,253,253,253,253,253,253,123,0,0,0,0,0,0,0,0,0,174,253,253,253,253,253,253,253,253,253,253,253,249,247,247,169,117,117,57,0,0,0,0,0,0,0,0,0,0,118,123,123,123,166,253,253,253,155,123,123,41,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"

four: String
four = "4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22,192,134,32,0,0,0,0,0,0,0,0,15,77,5,0,0,0,0,0,0,0,0,0,0,0,0,17,235,250,169,0,0,0,0,0,0,0,0,15,220,241,37,0,0,0,0,0,0,0,0,0,0,0,20,189,253,147,0,0,0,0,0,0,0,0,0,139,253,100,0,0,0,0,0,0,0,0,0,0,0,0,70,253,253,21,0,0,0,0,0,0,0,0,43,254,173,13,0,0,0,0,0,0,0,0,0,0,0,22,153,253,96,0,0,0,0,0,0,0,0,43,231,254,92,0,0,0,0,0,0,0,0,0,0,0,0,163,255,204,11,0,0,0,0,0,0,0,0,104,254,158,0,0,0,0,0,0,0,0,0,0,0,0,0,162,253,178,5,0,0,0,0,0,0,9,131,237,253,0,0,0,0,0,0,0,0,0,0,0,0,0,0,162,253,253,191,175,70,70,70,70,133,197,253,253,169,0,0,0,0,0,0,0,0,0,0,0,0,0,0,51,228,253,253,254,253,253,253,253,254,253,253,219,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,65,137,254,232,137,137,137,44,253,253,161,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,254,206,21,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,160,253,69,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85,254,241,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,158,254,165,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,231,244,50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,254,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,208,253,157,0,13,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,208,253,154,91,204,161,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,208,253,254,253,154,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61,190,128,23,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"

trainData: String -> (List Float, List Float)
trainData data =
    String.split "," data
        |> List.map String.toInt
        |> List.map (Result.withDefault -1)
        |> (\x -> (List.head x |> Maybe.withDefault -1, List.tail x |> Maybe.withDefault [] ))
       -- |> (\(x,m) -> (x, List.map toFloat m))
        |> (\(x, m) -> (target x, List.map (\x -> toFloat x / 255.0 * 0.99 |> (+) 0.01) m))

