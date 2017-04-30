module CSSOM exposing (..)

import Color exposing (..)
import CSSBasicTypes exposing (..)


type CSSDisplay
    = Block
    | Inline
    | None


type CSSMargin
    = MarginAuto
    | MarginLength CSSLength


type CSSPadding
    = PaddingLength CSSLength


type CSSHeight
    = HeightAuto
    | HeightLength CSSLength


type CSSWidth
    = WidthAuto
    | WidthLength CSSLength


type CSSBackgroundColor
    = BackgroundColorColor CSSColor
    | BackgroundColorTransparent


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
    | PaddingLeft CSSPadding
    | PaddingRight CSSPadding
    | PaddingTop CSSPadding
    | PaddingBottom CSSPadding
    | Height CSSHeight
    | Width CSSWidth
    | BackgroundColor CSSBackgroundColor
    | BorderLeftWidth CSSBorderWidth
    | BorderRightWidth CSSBorderWidth
    | BorderTopWidth CSSBorderWidth
    | BorderBottomWidth CSSBorderWidth


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


type CSSValue
    = Length Float CSSUnit
    | ColorValue CSSColor


type CSSColor
    = ColorKeyword CSSColorKeyword
    | RGBA Color


type CSSColorKeyword
    = Red
    | White


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
