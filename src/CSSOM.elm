module CSSOM exposing (..)

import Color exposing (..)


type CSSPropertyName
    = Display
    | MarginLeft
    | MarginRight
    | MarginBottom
    | MarginTop
    | PaddingLeft
    | PaddingRight
    | PaddingTop
    | PaddingBottom
    | Height
    | Width
    | BackgroundColor


type alias CSSDeclaration =
    { name : CSSPropertyName
    , value : CSSValue
    }


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
    = Keyword CSSKeyword
    | Length Float CSSUnit
    | ColorValue CSSColorValue


type CSSColorValue
    = ColorKeyword CSSColorKeyword
    | RGBA Color


type CSSColorKeyword
    = Red
    | White


type CSSKeyword
    = Auto
    | None
    | Block
    | Inline


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
