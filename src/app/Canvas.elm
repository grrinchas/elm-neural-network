module Canvas exposing (..)

import Color exposing (..)
import Graphics2D exposing (Point, Size)
import Html exposing (Attribute, Html)
import Html.Attributes
import Json.Encode
import Port


type alias Canvas =
    { id: String
    , size: Size
    , assets: List {id: String, url: String}
    }

initCanvas: Canvas -> Cmd msg
initCanvas = Port.initCanvas

onInitCanvas: (Canvas -> msg) -> Sub msg
onInitCanvas = Port.onInitCanvas

getImageData: Canvas -> Point -> Size -> Cmd msg
getImageData canvas point size =
    Port.getImageData {id = canvas.id, x = point.x, y = point.y, width = size.width, height = size.height}

onGetImageData: ({url: String, data: List Int} -> msg) -> Sub msg
onGetImageData = Port.onGetImageData
