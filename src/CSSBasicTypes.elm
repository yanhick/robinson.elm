module CSSBasicTypes
    exposing
        ( CSSLength
        , cssLength
        , CSSUnit(..)
        , computedCSSLength
        , defaultCSSLength
        )


type CSSLength
    = CSSLength Float CSSUnit


type CSSUnit
    = Pixel


cssLength : Float -> CSSUnit -> Maybe CSSLength
cssLength length unit =
    Just <| CSSLength length unit


defaultCSSLength : CSSLength
defaultCSSLength =
    CSSLength 0 Pixel


computedCSSLength : CSSLength -> Float
computedCSSLength (CSSLength length Pixel) =
    length
