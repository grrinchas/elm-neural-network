module Mouse exposing (..)

import Element exposing (Attribute)
import Element.Events exposing (Options, onWithOptions)
import Graphics2D exposing (Point)
import Json.Decode as Decode exposing (Decoder)

type alias Keys =
    { alt : Bool
    , ctrl : Bool
    , shift : Bool
    }

type Button
    = ErrorButton
    | MainButton
    | MiddleButton
    | SecondButton
    | BackButton
    | ForwardButton


type alias Event =
    { keys : Keys
    , button : Button
    , clientPos : Point
    , offsetPos : Point
    , pagePos : Point
    , screenPos : Point
    }


onDown : (Event -> msg) -> Attribute var msg
onDown =
    withOptions "mousedown" stopOptions


onMove : (Event -> msg) -> Attribute var msg
onMove =
    withOptions "mousemove" stopOptions


onUp : (Event -> msg) -> Attribute var msg
onUp =
    withOptions "mouseup" stopOptions


onClick : (Event -> msg) -> Attribute var msg
onClick =
    withOptions "click" stopOptions

onDoubleClick : (Event -> msg) -> Attribute var msg
onDoubleClick =
    withOptions "dblclick" stopOptions


onEnter : (Event -> msg) -> Attribute var msg
onEnter =
    withOptions "mouseenter" stopOptions


onOver : (Event -> msg) -> Attribute var msg
onOver =
    withOptions "mouseover" stopOptions


onLeave : (Event -> msg) -> Attribute var msg
onLeave =
    withOptions "mouseleave" stopOptions


onOut : (Event -> msg) -> Attribute var msg
onOut =
    withOptions "mouseout" stopOptions


onContextMenu : (Event -> msg) -> Attribute var msg
onContextMenu =
    withOptions "contextmenu" stopOptions


withOptions : String -> Options -> (Event -> msg) -> Attribute var msg
withOptions event options tag =
    Decode.map tag eventDecoder
        |> onWithOptions event options


stopOptions : Options
stopOptions =
    { stopPropagation = True
    , preventDefault = True
    }


eventDecoder : Decoder Event
eventDecoder =
    Decode.map6 Event
        keys
        buttonDecoder
        clientPos
        offsetPos
        pagePos
        screenPos



keys : Decoder Keys
keys =
    Decode.map3 Keys
        (Decode.field "altKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)


clientPos : Decoder Point
clientPos =
    Decode.map2 Graphics2D.point
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


offsetPos : Decoder Point
offsetPos =
    Decode.map2 Graphics2D.point
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


pagePos : Decoder Point
pagePos =
    Decode.map2 Graphics2D.point
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


screenPos : Decoder Point
screenPos =
    Decode.map2 Graphics2D.point
        (Decode.field "screenX" Decode.float)
        (Decode.field "screenY" Decode.float)



buttonDecoder : Decoder Button
buttonDecoder =
    Decode.map buttonFromId
        (Decode.field "button" Decode.int)


buttonFromId : Int -> Button
buttonFromId id =
    case id of
        0 ->
            MainButton

        1 ->
            MiddleButton

        2 ->
            SecondButton

        3 ->
            BackButton

        4 ->
            ForwardButton

        _ ->
            ErrorButton
