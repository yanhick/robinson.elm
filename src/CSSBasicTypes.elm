module CSSBasicTypes
    exposing
        ( CSSColor
        , CSSColorKeyword
        , CSSLength
        , CSSUnit
        , RGBAColor
        , computedCSSColor
        , computedCSSLength
        , cssColorFromColorName
        , cssColorFromRGBA
        , cssPixelLength
        , defaultCSSLength
        )


type CSSLength
    = CSSLength Float CSSUnit


type CSSUnit
    = Pixel


cssPixelLength : Float -> Maybe CSSLength
cssPixelLength length =
    Just <| CSSLength length Pixel


defaultCSSLength : CSSLength
defaultCSSLength =
    CSSLength 0 Pixel


computedCSSLength : CSSLength -> Float
computedCSSLength (CSSLength length Pixel) =
    length


type alias RGBAColor =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


cssColorFromRGBA : RGBAColor -> Maybe CSSColor
cssColorFromRGBA rgba =
    Just <| RGBA rgba


cssColorFromColorName : String -> Maybe CSSColor
cssColorFromColorName name =
    case name of
        "red" ->
            Just <| ColorKeyword Red

        "white" ->
            Just <| ColorKeyword White

        _ ->
            Nothing


computedCSSColor : CSSColor -> RGBAColor
computedCSSColor color =
    case color of
        RGBA rgba ->
            rgba

        ColorKeyword Red ->
            { red = 255, blue = 0, green = 0, alpha = 1.0 }

        ColorKeyword White ->
            { red = 255, blue = 255, green = 255, alpha = 1.0 }


type CSSColor
    = ColorKeyword CSSColorKeyword
    | RGBA RGBAColor


type CSSColorKeyword
    = Red
    | White
