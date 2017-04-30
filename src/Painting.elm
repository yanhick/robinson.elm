module Painting exposing (..)

import Color exposing (..)
import CSSOM
import Layout exposing (..)
import Style exposing (..)
import BoxModel


type DisplayCommand
    = SolidColor BoxModel.Rect Color


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
                            CSSOM.BackgroundColorColor (CSSOM.RGBA color) ->
                                Just color

                            _ ->
                                Nothing

                    _ ->
                        Nothing
    in
        [ renderBackground dimensions backgroundColor ] ++ (List.concatMap renderLayoutBox children)


renderBackground : BoxModel.BoxModel -> Color -> DisplayCommand
renderBackground boxModel color =
    SolidColor (BoxModel.content boxModel) color
