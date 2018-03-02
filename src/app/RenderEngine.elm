module RenderEngine exposing (..)

import Canvas exposing (Canvas)
import Color exposing (Color)
import Dict exposing (Dict)
import Graphics2D exposing (..)
import Json.Encode
import Port


render: Canvas -> List Operation -> Cmd msg
render canvas operations =
    Port.render <| encodeCanvas canvas operations


encodeCanvas: Canvas -> List Operation -> Json.Encode.Value
encodeCanvas canvas operations =
    Json.Encode.object
        [ ("id", Json.Encode.string canvas.id)
        , ("operations", Json.Encode.list <| List.map encodeOperation operations)
        ]


encodeOperation: Operation -> Json.Encode.Value
encodeOperation operation=
    case operation of
        Skip ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SKIP")
                ]
        BeginPath ->
            Json.Encode.object
                [ ("type", Json.Encode.string "BEGIN_PATH")
                ]
        ClosePath ->
            Json.Encode.object
                [ ("type", Json.Encode.string "CLOSE_PATH")
                ]
        FillPath rule ->
            Json.Encode.object
                [ ("type", Json.Encode.string "FILL_PATH")
                , ("value", encodeFillRule rule)
                ]
        StrokePath ->
            Json.Encode.object
                [ ("type", Json.Encode.string "STROKE_PATH")
                ]
        MoveTo point ->
            Json.Encode.object
                [ ("type", Json.Encode.string "MOVE_TO")
                , ("value", encodePoint point)
                ]
        LineTo point ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_TO")
                , ("value", encodePoint point)
                ]
        Arc center radius start end clock ->
            Json.Encode.object
                [ ("type", Json.Encode.string "ARC")
                , ("value", Json.Encode.object
                      [ ("x", Json.Encode.float center.x)
                      , ("y", Json.Encode.float center.y)
                      , ("radius", Json.Encode.float radius)
                      , ("startAngle", Json.Encode.float (degrees start))
                      , ("endAngle", Json.Encode.float (degrees end))
                      , ("anticlockwise", Json.Encode.bool <| isClockwise clock)
                      ]
                  )
                ]
        ArcTo fst snd radius ->
            Json.Encode.object
                [ ("type", Json.Encode.string "ARC_TO")
                , ("value", encodeArcTo fst snd radius)
                ]
        BezierCurveTo cp1 cp2 end ->
            Json.Encode.object
                [ ("type", Json.Encode.string "BEZIER_CURVE_TO")
                , ("value", bezierCurve cp1 cp2 end)
                ]
        QuadraticCurveTo cp end ->
            Json.Encode.object
                [ ("type", Json.Encode.string "QUADRATIC_CURVE_TO")
                , ("value", quadraticCurve cp end)
                ]
        Rect point size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "RECT")
                , ("value", encodeRect point size)
                ]
        FillRect point size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "FILL_RECT")
                , ("value", encodeRect point size)
                ]
        StrokeRect point size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "STROKE_RECT")
                , ("value", encodeRect point size)
                ]
        ClearRect point size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "CLEAR_RECT")
                , ("value", encodeRect point size)
                ]
        StrokeStyle colorStyle ->
            Json.Encode.object
                [ ("type", Json.Encode.string "STROKE_STYLE")
                , ("value", encodeStyle colorStyle)
                ]
        FillStyle colorStyle ->
            Json.Encode.object
                [ ("type", Json.Encode.string "FILL_STYLE")
                , ("value", encodeStyle colorStyle)
                ]
        GlobalAlpha alpha ->
            Json.Encode.object
                [ ("type", Json.Encode.string "GLOBAL_ALPHA")
                , ("value", Json.Encode.float alpha)
                ]
        LineWidth width ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_WIDTH")
                , ("value", Json.Encode.float width)
                ]
        LineCap cap ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_CAP")
                , ("value", encodeLineCap cap)
                ]
        LineJoin join ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_JOIN")
                , ("value", encodeLineJoin join)
                ]
        MiterLimit limit ->
            Json.Encode.object
                [ ("type", Json.Encode.string "MITER_LIMIT")
                , ("value", Json.Encode.float limit)
                ]

        LineDash values ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_DASH")
                , ("value", Json.Encode.list <| List.map Json.Encode.float values)
                ]
        LineDashOffset offset ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINE_DASH_OFFSET")
                , ("value", Json.Encode.float offset)
                ]
        DrawShadow x y blur color ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SHADOW")
                , ("value", Json.Encode.object
                    [ ("offsetX", Json.Encode.float x)
                    , ("offsetY", Json.Encode.float y)
                    , ("blur", Json.Encode.float blur)
                    , ("color", encodeColor color)
                    ]
                  )
                ]
        FillText text position width ->
            Json.Encode.object
                [ ("type", Json.Encode.string "FILL_TEXT")
                , ("value", Json.Encode.object
                    [ ("text", Json.Encode.string text)
                    , ("x", Json.Encode.float position.x)
                    , ("y", Json.Encode.float position.y)
                    , ("maxWidth", Json.Encode.float <| Maybe.withDefault -1 width)
                    ]
                  )
                ]

        StrokeText text position width->
            Json.Encode.object
                [ ("type", Json.Encode.string "STROKE_TEXT")
                , ("value", Json.Encode.object
                    [ ("text", Json.Encode.string text)
                    , ("x", Json.Encode.float position.x)
                    , ("y", Json.Encode.float position.y)
                    , ("maxWidth", Json.Encode.float <| Maybe.withDefault -1 width)
                    ]
                  )
                ]
        Font font ->
            Json.Encode.object
                [ ("type", Json.Encode.string "FONT")
                , ("value", Json.Encode.string font)
                ]
        TextAlign align ->
            Json.Encode.object
                [ ("type", Json.Encode.string "TEXT_ALIGN")
                , ("value", encodeAlign align)
                ]
        TextDirection dir ->
            Json.Encode.object
                [ ("type", Json.Encode.string "TEXT_DIRECTION")
                , ("value", encodeTextDirection dir)
                ]
        TextBaseline base ->
            Json.Encode.object
                [ ("type", Json.Encode.string "TEXT_BASELINE")
                , ("value", encodeBaseline base)
                ]
        DrawImage id point ->
            Json.Encode.object
                [ ("type", Json.Encode.string "DRAW_IMAGE")
                , ("value", Json.Encode.object
                    [ ("x", Json.Encode.float point.x)
                    , ("y", Json.Encode.float point.y)
                    , ("id", Json.Encode.string id)
                    ]
                  )
                ]

        ScaleImage id point size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SCALE_IMAGE")
                , ("value", Json.Encode.object
                    [ ("x", Json.Encode.float point.x)
                    , ("y", Json.Encode.float point.y)
                    , ("width", Json.Encode.float size.width)
                    , ("height", Json.Encode.float size.height)
                    , ("id", Json.Encode.string id)
                    ]
                  )
                ]
        SliceImage id sPoint sSize dPoint dSize ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SLICE_IMAGE")
                , ("value", Json.Encode.object
                    [ ("sx", Json.Encode.float sPoint.x)
                    , ("sy", Json.Encode.float sPoint.y)
                    , ("sWidth", Json.Encode.float sSize.width)
                    , ("sHeight", Json.Encode.float sSize.height)
                    , ("dx", Json.Encode.float dPoint.x)
                    , ("dy", Json.Encode.float dPoint.y)
                    , ("dWidth", Json.Encode.float dSize.width)
                    , ("dHeight", Json.Encode.float dSize.height)
                    , ("id", Json.Encode.string id)
                    ]
                  )
                ]

        Smoothing bool ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SMOOTHING")
                , ("value", Json.Encode.bool bool )
                ]
        Save ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SAVE")
                ]
        Restore ->
            Json.Encode.object
                [ ("type", Json.Encode.string "RESTORE")
                ]
        Translate point ->
            Json.Encode.object
                [ ("type", Json.Encode.string "TRANSLATE")
                , ("value", Json.Encode.object
                    [ ("x", Json.Encode.float point.x)
                    , ("y", Json.Encode.float point.y)
                    ]
                  )
                ]
        Rotate angle ->
            Json.Encode.object
                [ ("type", Json.Encode.string "ROTATE")
                , ("value", Json.Encode.float angle)
                ]
        Scale size ->
            Json.Encode.object
                [ ("type", Json.Encode.string "SCALE")
                , ("value", Json.Encode.object
                    [ ("width", Json.Encode.float size.width)
                    , ("height", Json.Encode.float size.height)
                    ]
                  )
                ]
        Transform a b c d e f ->
            Json.Encode.object
                [ ("type", Json.Encode.string "TRANSFORM")
                , ("value", Json.Encode.object
                    [ ("a", Json.Encode.float a)
                    , ("b", Json.Encode.float b)
                    , ("c", Json.Encode.float c)
                    , ("d", Json.Encode.float d)
                    , ("e", Json.Encode.float e)
                    , ("f", Json.Encode.float f)
                    ]
                  )
                ]
        Composition type_ ->
            Json.Encode.object
                [ ("type", Json.Encode.string "COMPOSITION")
                , ("value", encodeComposition type_)
                ]
        ClipPath rule ->
            Json.Encode.object
                [ ("type", Json.Encode.string "CLIP")
                , ("value", encodeFillRule rule)
                ]

encodeFillRule: FillRule -> Json.Encode.Value
encodeFillRule rule =
    case rule of
        NonZero -> Json.Encode.string "nonzero"
        EvenOdd -> Json.Encode.string "evenodd"


encodeComposition: CompositionType -> Json.Encode.Value
encodeComposition comp =
    case comp of
         SourceOver -> Json.Encode.string "source-over"
         SourceIn -> Json.Encode.string "source-in"
         SourceOut -> Json.Encode.string "source-out"
         SourceAtop -> Json.Encode.string "source-atop"
         DestinationOver -> Json.Encode.string "destination-over"
         DestinationIn -> Json.Encode.string "destination-in"
         DestinationOut -> Json.Encode.string "destination-out"
         DestinationAtop -> Json.Encode.string "destination-atop"
         Lighter -> Json.Encode.string "lighter"
         Copy -> Json.Encode.string "copy"
         XOR -> Json.Encode.string "xor"
         Multiply -> Json.Encode.string "multiply"
         Screen -> Json.Encode.string "screen"
         Overlay -> Json.Encode.string "overlay"
         Darken -> Json.Encode.string "darken"
         Lighten -> Json.Encode.string "lighten"
         ColorDodge -> Json.Encode.string "color-dodge"
         ColorBurn -> Json.Encode.string "color-burn"
         HardLight -> Json.Encode.string "hard-light"
         SoftLight -> Json.Encode.string "soft-light"
         Difference -> Json.Encode.string "difference"
         Exclusion -> Json.Encode.string "exclusion"
         Hue -> Json.Encode.string "hue"
         Saturation -> Json.Encode.string "saturation"
         ColorComposition -> Json.Encode.string "color"
         Luminosity -> Json.Encode.string "luminosity"


encodeTextDirection: Direction -> Json.Encode.Value
encodeTextDirection dir =
    case dir of
        LeftToRight -> Json.Encode.string "ltr"
        RightToLeft -> Json.Encode.string "rtl"
        Inherit -> Json.Encode.string "inherit"


encodeBaseline: Baseline -> Json.Encode.Value
encodeBaseline base =
    case base of
        Top -> Json.Encode.string "top"
        Hanging -> Json.Encode.string "hanging"
        Middle -> Json.Encode.string "middle"
        Alphabetic -> Json.Encode.string "alphabetic"
        Ideographic -> Json.Encode.string "ideographic"
        Bottom -> Json.Encode.string "bottom"


encodeAlign: Align -> Json.Encode.Value
encodeAlign align =
    case align of
        Start -> Json.Encode.string "start"
        End -> Json.Encode.string "end"
        Left -> Json.Encode.string "left"
        Right -> Json.Encode.string "right"
        Center -> Json.Encode.string "center"

encodeStyle: Style -> Json.Encode.Value
encodeStyle style =
    case style of
        PlainStyle color ->
            Json.Encode.object
                [ ("type", Json.Encode.string "PLAIN")
                , ("value", encodeColor color)
                ]
        LinearGradientStyle start end stops ->
            Json.Encode.object
                [ ("type", Json.Encode.string "LINEAR_GRADIENT")
                , ("value", Json.Encode.object
                     [ ("x1", Json.Encode.float start.x)
                     , ("y1", Json.Encode.float start.y)
                     , ("x2", Json.Encode.float end.x)
                     , ("y2", Json.Encode.float end.y)
                     , ("stops", Json.Encode.list <| List.map encodeStop stops)
                     ]
                  )
                ]
        RadialGradientStyle start r1 end r2 stops ->
            Json.Encode.object
                [ ("type", Json.Encode.string "RADIAL_GRADIENT")
                , ("value", Json.Encode.object
                     [ ("x1", Json.Encode.float start.x)
                     , ("y1", Json.Encode.float start.y)
                     , ("r1", Json.Encode.float r1)
                     , ("x2", Json.Encode.float end.x)
                     , ("y2", Json.Encode.float end.y)
                     , ("r2", Json.Encode.float r2)
                     , ("stops", Json.Encode.list <| List.map encodeStop stops)
                     ]
                  )
                ]
        PatternStyle id rep ->
            Json.Encode.object
                [ ("type", Json.Encode.string "PATTERN")
                , ("value",Json.Encode.object
                    [ ("repetition", encodeRepetition rep)
                    , ("id", Json.Encode.string id)
                    ]
                  )
                ]

encodeRepetition: Repetition -> Json.Encode.Value
encodeRepetition rep =
    case rep of
        Repeat -> Json.Encode.string "repeat"
        RepeatX -> Json.Encode.string "repeat-x"
        RepeatY -> Json.Encode.string "repeat-y"
        NoRepeat -> Json.Encode.string "no-repeat"



encodeStop: (Float, Color) -> Json.Encode.Value
encodeStop (value, color) =
    Json.Encode.object
        [ ("value", Json.Encode.float value)
        , ("color", encodeColor color)
        ]



encodeLineJoin : Join -> Json.Encode.Value
encodeLineJoin join =
    case join of
        BevelJoin-> Json.Encode.string "bevel"
        RoundJoin -> Json.Encode.string "round"
        MiterJoin -> Json.Encode.string "miter"

encodeLineCap : Cap -> Json.Encode.Value
encodeLineCap cap =
    case cap of
        ButtCap -> Json.Encode.string "butt"
        RoundCap -> Json.Encode.string "round"
        SquareCap -> Json.Encode.string "square"

quadraticCurve: Point -> Point -> Json.Encode.Value
quadraticCurve cp {x, y} =
    Json.Encode.object
        [ ("cpx", Json.Encode.float cp.x)
        , ("cpy", Json.Encode.float cp.y)
        , ("x", Json.Encode.float x)
        , ("y", Json.Encode.float y)
        ]

bezierCurve: Point -> Point -> Point -> Json.Encode.Value
bezierCurve cp1 cp2 {x, y} =
    Json.Encode.object
        [ ("cp1x", Json.Encode.float cp1.x)
        , ("cp1y", Json.Encode.float cp1.y)
        , ("cp2x", Json.Encode.float cp2.x)
        , ("cp2y", Json.Encode.float cp2.y)
        , ("x", Json.Encode.float x)
        , ("y", Json.Encode.float y)
        ]


encodeArcTo: Point -> Point -> Float -> Json.Encode.Value
encodeArcTo fst snd radius =
    Json.Encode.object
        [ ("x1", Json.Encode.float fst.x)
        , ("y1", Json.Encode.float fst.y)
        , ("x2", Json.Encode.float snd.x)
        , ("y2", Json.Encode.float snd.y)
        , ("radius", Json.Encode.float radius)
        ]




encodeRect: Point -> Size -> Json.Encode.Value
encodeRect {x, y} {width, height} =
    Json.Encode.object
        [ ("x", Json.Encode.float x)
        , ("y", Json.Encode.float y)
        , ("width", Json.Encode.float width)
        , ("height", Json.Encode.float height)
        ]


encodePoint: Point-> Json.Encode.Value
encodePoint {x, y} =
    Json.Encode.object
        [ ("x", Json.Encode.float x)
        , ("y", Json.Encode.float y)
        ]

encodeColor: Color -> Json.Encode.Value
encodeColor color =
    case Color.toRgb color of
        {red, green, blue, alpha} ->
            String.concat
                [ "rgba("
                , toString red
                , ","
                , toString green
                , ","
                , toString blue
                , ","
                , toString alpha
                , ")"
                ] |> Json.Encode.string
