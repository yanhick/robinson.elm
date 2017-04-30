module CSSOM exposing (..)

import Color exposing (..)


type CSSPropertyName
    = MarginRight
    | MarginBottom
    | MarginTop
    | PaddingLeft
    | PaddingRight
    | PaddingTop
    | PaddingBottom
    | Height
    | Width
    | BackgroundColor


type CSSDisplay
    = Block
    | Inline
    | None


type CSSMargin
    = MarginAuto
    | MarginLength CSSLength


type CSSLength
    = CSSLength Float CSSUnit


type CSSDeclaration
    = Display CSSDisplay
    | MarginLeft CSSMargin


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


type CSSUnit
    = Pixel


type CSSValue
    = Length Float CSSUnit
    | ColorValue CSSColorValue


type CSSColorValue
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
