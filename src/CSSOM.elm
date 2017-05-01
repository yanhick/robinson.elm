module CSSOM
    exposing
        ( CSSDisplay(..)
        , CSSMargin(..)
        , CSSPadding
        , defaultPadding
        , computedPadding
        , usedPadding
        , SpecifiedValue
        , padding
        , CSSHeight(..)
        , CSSWidth(..)
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


type CSSDisplay
    = Block
    | Inline
    | None


type CSSMargin
    = MarginAuto
    | MarginLength CSSLength


type SpecifiedValue
    = SpecifiedValue


type ComputedValue
    = ComputedValue


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


type CSSHeight
    = HeightAuto
    | HeightLength CSSLength


type CSSWidth
    = WidthAuto
    | WidthLength CSSLength


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
    | MarginLeft CSSMargin
    | MarginRight CSSMargin
    | MarginTop CSSMargin
    | MarginBottom CSSMargin
    | PaddingLeft (CSSPadding SpecifiedValue)
    | PaddingRight (CSSPadding SpecifiedValue)
    | PaddingTop (CSSPadding SpecifiedValue)
    | PaddingBottom (CSSPadding SpecifiedValue)
    | Height CSSHeight
    | Width CSSWidth
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
