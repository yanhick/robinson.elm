module CSSOM
    exposing
        ( CSSDisplay(..)
        , CSSMargin
        , marginLength
        , marginAuto
        , defaultMargin
        , isAutoMargin
        , computedMargin
        , usedMargin
        , CSSPadding
        , defaultPadding
        , computedPadding
        , usedPadding
        , SpecifiedValue
        , padding
        , CSSHeight
        , defaultHeight
        , heightLength
        , heightAuto
        , computedHeight
        , usedHeight
        , CSSWidth
        , defaultWidth
        , widthLength
        , widthAuto
        , computedWidth
        , isAutoWidth
        , usedWidth
        , CSSBackgroundColor
        , backgroundColorColor
        , backgroundColorTransparent
        , defaultBackgroundColor
        , usedBackgroundColor
        , CSSBorderColor
        , borderColorColor
        , borderColorTransparent
        , defaultBorderColor
        , usedBorderColor
        , CSSBorderWidth
        , borderWidthThin
        , borderWidthMedium
        , borderWidthThick
        , borderWidthLength
        , computedBorderWidth
        , usedBorderWidth
        , defaultBorderWidth
        , CSSBorderStyle
        , borderStyleNone
        , borderStyleSolid
        , defaultBorderStyle
        , CSSDeclaration(..)
        , CSSRule
        , CSSSelector(..)
        , CSSSimpleSelector
        , specifity
        , CSSStyleSheet
        )

import CSSBasicTypes exposing (..)


type SpecifiedValue
    = SpecifiedValue


type ComputedValue
    = ComputedValue


type CSSDisplay
    = Block
    | Inline
    | None


type CSSMargin valueType
    = MarginAuto
    | MarginLength CSSLength


marginLength : CSSLength -> CSSMargin SpecifiedValue
marginLength =
    MarginLength


marginAuto : CSSMargin SpecifiedValue
marginAuto =
    MarginAuto


defaultMargin : CSSMargin SpecifiedValue
defaultMargin =
    MarginLength defaultCSSLength


isAutoMargin : CSSMargin valueType -> Bool
isAutoMargin margin =
    case margin of
        MarginAuto ->
            True

        _ ->
            False


computedMargin : CSSMargin SpecifiedValue -> CSSMargin ComputedValue
computedMargin margin =
    case margin of
        MarginLength length ->
            MarginLength length

        MarginAuto ->
            MarginAuto


usedMargin : CSSMargin ComputedValue -> Maybe Float
usedMargin margin =
    case margin of
        MarginLength length ->
            Just <| computedCSSLength length

        MarginAuto ->
            Nothing


type CSSPadding valueType
    = PaddingLength CSSLength


defaultPadding : CSSPadding SpecifiedValue
defaultPadding =
    PaddingLength defaultCSSLength


padding : CSSLength -> CSSPadding SpecifiedValue
padding =
    PaddingLength


computedPadding : CSSPadding SpecifiedValue -> CSSPadding ComputedValue
computedPadding (PaddingLength length) =
    PaddingLength length


usedPadding : CSSPadding ComputedValue -> Float
usedPadding (PaddingLength length) =
    computedCSSLength length


type CSSHeight valueType
    = HeightAuto
    | HeightLength CSSLength


heightLength : CSSLength -> CSSHeight SpecifiedValue
heightLength =
    HeightLength


heightAuto : CSSHeight SpecifiedValue
heightAuto =
    HeightAuto


defaultHeight : CSSHeight SpecifiedValue
defaultHeight =
    HeightAuto


computedHeight : CSSHeight SpecifiedValue -> CSSHeight ComputedValue
computedHeight height =
    case height of
        HeightAuto ->
            HeightAuto

        HeightLength length ->
            HeightLength length


usedHeight : CSSHeight ComputedValue -> Maybe Float
usedHeight height =
    case height of
        HeightAuto ->
            Nothing

        HeightLength length ->
            Just <| computedCSSLength length


type CSSWidth valueType
    = WidthAuto
    | WidthLength CSSLength


widthLength : CSSLength -> CSSWidth SpecifiedValue
widthLength =
    WidthLength


widthAuto : CSSWidth SpecifiedValue
widthAuto =
    WidthAuto


isAutoWidth : CSSWidth valueType -> Bool
isAutoWidth width =
    case width of
        WidthAuto ->
            True

        _ ->
            False


computedWidth : CSSWidth SpecifiedValue -> CSSWidth ComputedValue
computedWidth width =
    case width of
        WidthAuto ->
            WidthAuto

        WidthLength length ->
            WidthLength length


defaultWidth : CSSWidth SpecifiedValue
defaultWidth =
    WidthAuto


usedWidth : CSSWidth ComputedValue -> Maybe Float
usedWidth width =
    case width of
        WidthAuto ->
            Nothing

        WidthLength length ->
            Just <| computedCSSLength length


type CSSBackgroundColor valueType
    = BackgroundColorColor CSSColor
    | BackgroundColorTransparent


backgroundColorColor : CSSColor -> CSSBackgroundColor SpecifiedValue
backgroundColorColor =
    BackgroundColorColor


backgroundColorTransparent : CSSBackgroundColor SpecifiedValue
backgroundColorTransparent =
    BackgroundColorTransparent


defaultBackgroundColor : CSSBackgroundColor SpecifiedValue
defaultBackgroundColor =
    BackgroundColorTransparent


usedBackgroundColor : CSSBackgroundColor SpecifiedValue -> RGBAColor
usedBackgroundColor backgroundColor =
    case backgroundColor of
        BackgroundColorColor color ->
            computedCSSColor color

        BackgroundColorTransparent ->
            { red = 0
            , green = 0
            , blue = 0
            , alpha = 0
            }


type CSSBorderStyle valueType
    = BorderStyleNone
    | BorderStyleSolid


borderStyleNone : CSSBorderStyle SpecifiedValue
borderStyleNone =
    BorderStyleNone


borderStyleSolid : CSSBorderStyle SpecifiedValue
borderStyleSolid =
    BorderStyleSolid


defaultBorderStyle : CSSBorderStyle SpecifiedValue
defaultBorderStyle =
    BorderStyleNone


type CSSBorderColor valueType
    = BorderColorColor CSSColor
    | BorderColorTransparent


borderColorColor : CSSColor -> CSSBorderColor SpecifiedValue
borderColorColor =
    BorderColorColor


borderColorTransparent : CSSBorderColor SpecifiedValue
borderColorTransparent =
    BorderColorTransparent


defaultBorderColor : CSSBorderColor SpecifiedValue
defaultBorderColor =
    BorderColorTransparent


usedBorderColor : CSSBorderColor SpecifiedValue -> RGBAColor
usedBorderColor borderColor =
    case borderColor of
        BorderColorColor color ->
            computedCSSColor color

        BorderColorTransparent ->
            { red = 0
            , green = 0
            , blue = 0
            , alpha = 0
            }


type CSSBorderWidth valueType
    = BorderWidthThin
    | BorderWidthMedium
    | BorderWidthThick
    | BorderWidthLength CSSLength


defaultBorderWidth : CSSBorderWidth SpecifiedValue
defaultBorderWidth =
    BorderWidthMedium


computedBorderWidth :
    CSSBorderStyle SpecifiedValue
    -> CSSBorderWidth SpecifiedValue
    -> CSSBorderWidth ComputedValue
computedBorderWidth borderStyle borderWidth =
    case borderStyle of
        BorderStyleNone ->
            BorderWidthLength defaultCSSLength

        BorderStyleSolid ->
            case borderWidth of
                BorderWidthThin ->
                    BorderWidthThin

                BorderWidthMedium ->
                    BorderWidthMedium

                BorderWidthThick ->
                    BorderWidthThick

                BorderWidthLength length ->
                    BorderWidthLength length


usedBorderWidth : CSSBorderWidth ComputedValue -> Float
usedBorderWidth borderWidth =
    case borderWidth of
        BorderWidthThin ->
            0

        BorderWidthMedium ->
            0

        BorderWidthThick ->
            0

        BorderWidthLength length ->
            computedCSSLength length


borderWidthThick : CSSBorderWidth SpecifiedValue
borderWidthThick =
    BorderWidthThick


borderWidthMedium : CSSBorderWidth SpecifiedValue
borderWidthMedium =
    BorderWidthMedium


borderWidthThin : CSSBorderWidth SpecifiedValue
borderWidthThin =
    BorderWidthThin


borderWidthLength : CSSLength -> CSSBorderWidth SpecifiedValue
borderWidthLength =
    BorderWidthLength


type CSSDeclaration
    = Display CSSDisplay
    | MarginLeft (CSSMargin SpecifiedValue)
    | MarginRight (CSSMargin SpecifiedValue)
    | MarginTop (CSSMargin SpecifiedValue)
    | MarginBottom (CSSMargin SpecifiedValue)
    | PaddingLeft (CSSPadding SpecifiedValue)
    | PaddingRight (CSSPadding SpecifiedValue)
    | PaddingTop (CSSPadding SpecifiedValue)
    | PaddingBottom (CSSPadding SpecifiedValue)
    | Height (CSSHeight SpecifiedValue)
    | Width (CSSWidth SpecifiedValue)
    | BackgroundColor (CSSBackgroundColor SpecifiedValue)
    | BorderLeftWidth (CSSBorderWidth SpecifiedValue)
    | BorderRightWidth (CSSBorderWidth SpecifiedValue)
    | BorderTopWidth (CSSBorderWidth SpecifiedValue)
    | BorderBottomWidth (CSSBorderWidth SpecifiedValue)
    | BorderTopColor (CSSBorderColor SpecifiedValue)
    | BorderBottomColor (CSSBorderColor SpecifiedValue)
    | BorderLeftColor (CSSBorderColor SpecifiedValue)
    | BorderRightColor (CSSBorderColor SpecifiedValue)
    | BorderLeftStyle (CSSBorderStyle SpecifiedValue)
    | BorderRightStyle (CSSBorderStyle SpecifiedValue)
    | BorderTopStyle (CSSBorderStyle SpecifiedValue)
    | BorderBottomStyle (CSSBorderStyle SpecifiedValue)


type alias CSSRule =
    { selectors : List CSSSelector
    , declarations : List CSSDeclaration
    }


type alias CSSSimpleSelector =
    { tag : Maybe String
    , classes : List String
    , ids : List String
    }


type CSSSelector
    = Simple CSSSimpleSelector
    | Universal


type alias CSSStyleSheet =
    List CSSRule


specifity : CSSSelector -> Int
specifity selector =
    case selector of
        Simple { tag, classes, ids } ->
            List.length classes
                + List.length ids
                + Maybe.withDefault 0
                    (Maybe.map (always 1) tag)

        Universal ->
            0
