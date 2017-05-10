module LayoutBox exposing (..)

import Style exposing (..)
import BoxModel exposing (..)


type alias Box =
    { boxModel : BoxModel
    , styles : Styles
    , children : List LayoutBox
    }


type LayoutBox
    = BlockBox Box
    | InlineBox Box
    | AnonymousBoxInlineRoot Box
    | AnonymousBox Box
    | TextBox String


type InlineLevelElement
    = InlineContainer Styles (List AnonymizedBox)
    | InlineText String
    | AnonymousInlineRoot (List AnonymizedBox)


type BlockLevelElement
    = BlockContainer Styles (List AnonymizedBox)
    | AnonymousBlock (List AnonymizedBox)


type AnonymizedBox
    = BlockLevel BlockLevelElement
    | InlineLevel InlineLevelElement
