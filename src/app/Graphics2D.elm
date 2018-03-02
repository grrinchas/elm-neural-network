module Graphics2D exposing (..)

import Color exposing (Color)

type alias Point =
    { x: Float
    , y: Float
    }


point: Float -> Float -> Point
point x y = Point x y


type alias Size =
    { width: Float
    , height: Float
    }


size: Float -> Float -> Size
size width height = Size width height


type ClockDirection
    = Clockwise
    | Anticlockwise


clockwise: ClockDirection
clockwise = Clockwise

anticlockwise: ClockDirection
anticlockwise = Anticlockwise

isClockwise: ClockDirection -> Bool
isClockwise dir = dir == Clockwise


type Cap
    = ButtCap
    | RoundCap
    | SquareCap

type Join
    = RoundJoin
    | BevelJoin
    | MiterJoin

type Style
    = PlainStyle Color
    | LinearGradientStyle Point Point (List (Float, Color))
    | RadialGradientStyle Point Float Point Float (List (Float, Color))
    | PatternStyle String Repetition

type Repetition
    = Repeat
    | RepeatX
    | RepeatY
    | NoRepeat

repeat: Repetition
repeat =  Repeat

repeatX: Repetition
repeatX =  RepeatX

repeatY: Repetition
repeatY =  RepeatY

noRepeat: Repetition
noRepeat =  NoRepeat

type FillRule
    = NonZero
    | EvenOdd

nonZero: FillRule
nonZero = NonZero

evenOdd: FillRule
evenOdd = EvenOdd

type Align
    = Start
    | End
    | Left
    | Right
    | Center

start: Align
start = Start

end: Align
end = End

left: Align
left = Left

right: Align
right = Right

center: Align
center = Center

type Baseline
    = Top
    | Hanging
    | Middle
    | Alphabetic
    | Ideographic
    | Bottom

top: Baseline
top = Top

bottom:Baseline
bottom = Bottom

hanging: Baseline
hanging = Hanging

middle : Baseline
middle = Middle

alphabetic: Baseline
alphabetic = Alphabetic

ideographic: Baseline
ideographic = Ideographic

type Direction
    = LeftToRight
    | RightToLeft
    | Inherit


leftToRight: Direction
leftToRight = LeftToRight

rightToLeft: Direction
rightToLeft = RightToLeft

inherit: Direction
inherit = Inherit


type CompositionType
    = SourceOver
    | SourceIn
    | SourceOut
    | SourceAtop
    | DestinationOver
    | DestinationIn
    | DestinationOut
    | DestinationAtop
    | Lighter
    | Copy
    | XOR
    | Multiply
    | Screen
    | Overlay
    | Darken
    | Lighten
    | ColorDodge
    | ColorBurn
    | HardLight
    | SoftLight
    | Difference
    | Exclusion
    | Hue
    | Saturation
    | ColorComposition
    | Luminosity

sourceOver: CompositionType
sourceOver = SourceOver

sourceIn: CompositionType
sourceIn = SourceIn

sourceOut: CompositionType
sourceOut = SourceOut

sourceAtop: CompositionType
sourceAtop = DestinationAtop

destinationOver: CompositionType
destinationOver = DestinationOver

destinationIn: CompositionType
destinationIn = DestinationIn

destinationOut: CompositionType
destinationOut = DestinationOut

destinationAtop: CompositionType
destinationAtop = DestinationAtop

lighter: CompositionType
lighter = Lighter

copy: CompositionType
copy = Copy

xor: CompositionType
xor = XOR

multiply: CompositionType
multiply = Multiply

screen: CompositionType
screen = Screen

overlay: CompositionType
overlay = Overlay

darken: CompositionType
darken = Darken

lighten: CompositionType
lighten = Lighten

colorDodge: CompositionType
colorDodge = ColorDodge

colorBurn: CompositionType
colorBurn = ColorBurn

hardLight: CompositionType
hardLight = HardLight

softLight: CompositionType
softLight = SoftLight

difference: CompositionType
difference = Difference

exclusion: CompositionType
exclusion = Exclusion

hue: CompositionType
hue = Hue

saturation: CompositionType
saturation = Saturation

colorComposition: CompositionType
colorComposition = ColorComposition

luminosity: CompositionType
luminosity = Luminosity

type Operation
    = Skip
    | BeginPath
    | ClosePath
    | FillPath FillRule
    | StrokePath
    | MoveTo Point
    | LineTo Point
    | Arc Point Float Float Float ClockDirection
    | ArcTo Point Point Float
    | QuadraticCurveTo Point Point
    | BezierCurveTo Point Point Point
    | Rect Point Size

    -- Rectangles
    | FillRect Point Size
    | StrokeRect Point Size
    | ClearRect Point Size

    -- Colors
    | FillStyle Style
    | StrokeStyle Style
    | GlobalAlpha Float

    -- Lines
    | LineWidth Float
    | LineCap Cap
    | LineJoin Join
    | MiterLimit Float
    | LineDash (List Float)
    | LineDashOffset Float

    -- Images
    | DrawImage String Point
    | ScaleImage String Point Size
    | SliceImage String Point Size Point Size
    | Smoothing Bool

    -- Shadows
    | DrawShadow Float Float Float Color

    -- Text
    | FillText String Point (Maybe Float)
    | StrokeText String Point (Maybe Float)
    | Font String
    | TextAlign Align
    | TextBaseline Baseline
    | TextDirection Direction
    | Save
    | Restore

    -- Transformations
    | Translate Point
    | Rotate Float
    | Scale Size
    | Transform Float Float Float Float Float Float

    -- Composition
    | Composition CompositionType
    | ClipPath FillRule


skip: Operation
skip = Skip

clipPathWith: FillRule -> Operation
clipPathWith = ClipPath

clipPath: Operation
clipPath = clipPathWith NonZero

composition: CompositionType -> Operation
composition = Composition

transform: Float -> Float -> Float -> Float -> Float -> Float -> Operation
transform = Transform

scale: Size -> Operation
scale = Scale

rotate: Float -> Operation
rotate angle = Rotate <| degrees angle

translate: Point -> Operation
translate = Translate

save: Operation
save = Save

restore: Operation
restore = Restore

smoothing: Bool -> Operation
smoothing = Smoothing


sliceImage: String -> Point -> Size -> Point -> Size -> Operation
sliceImage = SliceImage

scaleImage: String -> Point -> Size -> Operation
scaleImage = ScaleImage

drawImage: String -> Point -> Operation
drawImage = DrawImage

strokeText: String -> Point -> Maybe Float -> Operation
strokeText = StrokeText

font: String -> Operation
font = Font

textBaseline: Baseline -> Operation
textBaseline = TextBaseline

textAlign: Align -> Operation
textAlign = TextAlign

textDirection: Direction -> Operation
textDirection = TextDirection

fillText: String -> Point -> Maybe Float -> Operation
fillText = FillText

shadow: Float -> Float -> Float -> Color -> Operation
shadow = DrawShadow

pattern: String -> Repetition -> Style
pattern = PatternStyle


plain: Color -> Style
plain = PlainStyle

linearGradient: Point -> Point -> List (Float, Color) -> Style
linearGradient = LinearGradientStyle

radialGradient: Point -> Float -> Point -> Float -> List (Float, Color) -> Style
radialGradient = RadialGradientStyle

lineDashOffset: Float -> Operation
lineDashOffset = LineDashOffset

lineDash: List Float -> Operation
lineDash = LineDash

miterLimit: Float -> Operation
miterLimit = MiterLimit

roundJoin: Operation
roundJoin = LineJoin RoundJoin

bevelJoin: Operation
bevelJoin = LineJoin BevelJoin

miterJoin: Operation
miterJoin = LineJoin MiterJoin

buttCap: Operation
buttCap = LineCap ButtCap

roundCap: Operation
roundCap = LineCap RoundCap

squareCap : Operation
squareCap = LineCap SquareCap

globalAlpha : Float -> Operation
globalAlpha = GlobalAlpha

lineWidth: Float -> Operation
lineWidth = LineWidth

fillStyle: Style -> Operation
fillStyle = FillStyle

strokeStyle: Style -> Operation
strokeStyle = StrokeStyle


rect: Point -> Size -> Operation
rect = Rect

bezierCurveTo: Point -> Point -> Point -> Operation
bezierCurveTo = BezierCurveTo

quadraticCurveTo: Point -> Point -> Operation
quadraticCurveTo = QuadraticCurveTo

arcTo: Point -> Point -> Float -> Operation
arcTo = ArcTo

moveTo: Point -> Operation
moveTo = MoveTo

lineTo: Point -> Operation
lineTo = LineTo

arc: Point -> Float -> Float -> Float -> ClockDirection -> Operation
arc = Arc


beginPath: Operation
beginPath = BeginPath

closePath : Operation
closePath = ClosePath


fillPathWith: FillRule -> Operation
fillPathWith = FillPath


fillPath: Operation
fillPath = FillPath NonZero

strokePath : Operation
strokePath = StrokePath

fillRect: Point -> Size -> Operation
fillRect = FillRect

strokeRect: Point -> Size -> Operation
strokeRect = StrokeRect

clearRect: Point -> Size -> Operation
clearRect = ClearRect
