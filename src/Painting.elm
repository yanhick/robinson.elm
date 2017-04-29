module Painting exposing (..)

import Color exposing (..)
import Layout exposing (..)
import Style exposing (..)


type DisplayCommand
    = SolidColor Rect Color


buildDisplayList : LayoutBox -> List DisplayCommand
buildDisplayList layoutBox =
    renderLayoutBox (layoutBox)


renderLayoutBox : LayoutBox -> List DisplayCommand
renderLayoutBox (LayoutBox { dimensions, children, box }) =
    let
        backgroundColor =
            Maybe.withDefault
                (rgba 0 0 0 0.0)
            <|
                case box of
                    Layout.Block (StyledElement { styles }) ->
                        case styles.backgroundColor of
                            CSSColor color ->
                                Just color

                            _ ->
                                Nothing

                    _ ->
                        Nothing
    in
        [ renderBackground dimensions backgroundColor ] ++ (List.concatMap renderLayoutBox children)


renderBackground : Dimensions -> Color -> DisplayCommand
renderBackground dimensions color =
    SolidColor dimensions.content color
