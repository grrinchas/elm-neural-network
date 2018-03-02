port module Port exposing (..)

import Array exposing (Array)
import Graphics2D exposing (Size)
import Json.Encode


port initCanvas: {id: String, size: Size, assets: List {id: String, url: String}} -> Cmd msg

port onInitCanvas: ({id: String, size: Size, assets: List {id:String, url: String}} -> msg) -> Sub msg

port render: Json.Encode.Value -> Cmd msg

port getImageData: {id: String, x: Float, y: Float, width: Float, height: Float} -> Cmd msg

port onGetImageData: (List Int -> msg) -> Sub msg

port copyCanvas: {source: String, destination: String} -> Cmd msg
