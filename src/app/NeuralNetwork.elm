module NeuralNetwork exposing (..)

import Matrix exposing (Matrix)
import Random

type alias Inputs = List Float
type alias Targets = List Float

type alias NeuralNetwork =
    { input: Int
    , output: Int
    , hidden: Int
    , learningRate: Float
    , wih: Matrix Float
    , woh: Matrix Float
    }



randomize: (NeuralNetwork -> msg ) -> NeuralNetwork -> Cmd msg
randomize msg n =
    let wih = n.hidden * n.input
        woh = n.output * n.hidden
        total = wih + woh
    in
    Random.float -0.5 0.5
        |> Random.list total
        |> Random.map (\list -> ({n | wih = Matrix.fromList n.input <| List.take wih list}, List.drop wih list))
        |> Random.map (\(n, list) -> {n | woh = Matrix.fromList n.hidden list})
        |> Random.generate msg


gaussian: Float -> Float -> Float -> Float
gaussian mean deviation x =
    (e^(-((x - mean)^2)/(2 * deviation ^ 2)))/(sqrt (2 * pi * deviation ^ 2))

sigmoid: Float -> Float
sigmoid x = 1 / (1 + e^(-x))


query: Inputs -> NeuralNetwork -> Matrix Float
query inputs network =
    Matrix.colMatrix inputs
        |> Matrix.unsafeDot network.wih
        |> Matrix.map sigmoid
        |> Matrix.unsafeDot network.woh
        |> Matrix.map sigmoid


train: (Targets, Inputs) -> NeuralNetwork -> NeuralNetwork
train (t,i) network =
    let inputs = Matrix.colMatrix i
        targets = Matrix.colMatrix t
        hiddenOutputs = Matrix.unsafeDot network.wih inputs |> Matrix.map sigmoid
        finalOutputs = Matrix.unsafeDot network.woh hiddenOutputs |> Matrix.map sigmoid
        finalErrors = Matrix.subtract targets finalOutputs
        hiddenErrors = Matrix.unsafeDot (Matrix.transpose network.woh) finalErrors
    in
    { input = network.input
    , output = network.output
    , hidden = network.hidden
    , learningRate = network.learningRate
    , woh =
        Matrix.map (\x -> 1 - x) finalOutputs
            |> Matrix.multiply finalOutputs
            |> Matrix.multiply finalErrors
            |> (\x -> Matrix.unsafeDot x <| Matrix.transpose hiddenOutputs)
            |> Matrix.map ((*) network.learningRate)
            |> Matrix.add network.woh
    , wih =
        Matrix.map (\x -> 1 - x) hiddenOutputs
            |> Matrix.multiply hiddenOutputs
            |> Matrix.multiply hiddenErrors
            |> (\x -> Matrix.unsafeDot x <| Matrix.transpose inputs)
            |> Matrix.map ((*) network.learningRate)
            |> Matrix.add network.wih
    }


