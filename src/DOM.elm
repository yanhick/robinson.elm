module DOM
    exposing
        ( Attributes
        , DOMNode(Element, Text)
        , DOMRoot(DOMRoot)
        , ElementNode
        , TagName
        )

import Dict


type alias TagName =
    String


type alias Attributes =
    Dict.Dict String String


type alias ElementNode =
    { children : List DOMNode
    , tagName : String
    , attributes : Attributes
    }


type DOMRoot
    = DOMRoot ElementNode


type DOMNode
    = Text String
    | Element ElementNode
