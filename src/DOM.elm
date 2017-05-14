module DOM exposing (..)

import Dict exposing (..)


type alias TagName =
    String


type alias Attribute =
    String


type alias Attributes =
    Dict String String


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
