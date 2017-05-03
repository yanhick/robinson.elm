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
        , CSSBackgroundColor(..)
        , CSSBorderColor(..)
        , CSSBorderWidth(..)
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


type CSSBackgroundColor
    = BackgroundColorColor CSSColor
    | BackgroundColorTransparent


type CSSBorderColor
    = BorderColorColor CSSColor
    | BorderColorTransparent


type CSSBorderWidth
    = BorderWidthThin
    | BorderWidthMedium
    | BorderWidthThick
    | BorderWidthLength CSSLength


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
    | BackgroundColor CSSBackgroundColor
    | BorderLeftWidth CSSBorderWidth
    | BorderRightWidth CSSBorderWidth
    | BorderTopWidth CSSBorderWidth
    | BorderBottomWidth CSSBorderWidth
    | BorderTopColor CSSBorderColor
    | BorderBottomColor CSSBorderColor
    | BorderLeftColor CSSBorderColor
    | BorderRightColor CSSBorderColor


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
