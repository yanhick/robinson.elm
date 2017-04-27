module Painting exposing (..)

import Color exposing (..)
import Layout exposing (..)


type DisplayCommand
    = SolidColor Rect


buildDisplayList : LayoutBox -> List DisplayCommand
buildDisplayList layoutBox =
    renderLayoutBox (layoutBox)


renderLayoutBox : LayoutBox -> List DisplayCommand
renderLayoutBox (LayoutBox { dimensions, children }) =
    [ renderBackground dimensions ] ++ (List.concatMap renderLayoutBox children)


renderBackground : Dimensions -> DisplayCommand
renderBackground dimensions =
    SolidColor dimensions.content
