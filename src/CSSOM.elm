module CSSOM exposing (..)

import Color exposing (..)


type alias CSSColor =
    Color


type alias CSSDeclaration =
    { name : String
    , value : CSSValue
    }


type alias CSSRule =
    { selectors : List CSSSelector
    , declarations : List CSSDeclaration
    }


type CSSSimpleSelector
    = Id String
    | Class String
    | Tag String
    | Universal


type CSSSelector
    = Simple (List CSSSimpleSelector)


type alias CSSStyleSheet =
    List CSSRule


type CSSUnit
    = Pixel


type CSSValue
    = Keyword String
    | Length Float CSSUnit
    | ColorValue Color
