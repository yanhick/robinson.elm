module DOM exposing (..)

import Dict exposing (..)


type alias TagName =
    String


type Attribute
    = Attribute String String


type alias Attributes =
    List Attribute


type DOMNode
    = Text String
    | Element
        { children : List DOMNode
        , tagName : String
        , attributes : Attributes
        }
