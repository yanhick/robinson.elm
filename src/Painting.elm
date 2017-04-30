module Painting exposing (..)

import Color exposing (..)
import CSSOM
import Layout exposing (..)
import Style exposing (..)
import BoxModel
import CSSBasicTypes


type DisplayCommand
    = SolidColor BoxModel.Rect CSSBasicTypes.RGBAColor


buildDisplayList : LayoutBox -> List DisplayCommand
buildDisplayList layoutBox =
    renderLayoutBox (layoutBox)


renderLayoutBox : LayoutBox -> List DisplayCommand
renderLayoutBox (LayoutBox { dimensions, children, box }) =
    let
        backgroundColor =
            Maybe.withDefault
                { red = 0, green = 0, blue = 0, alpha = 0 }
            <|
                case box of
                    Layout.Block (StyledElement { styles }) ->
                        case styles.backgroundColor of
                            CSSOM.BackgroundColorColor color ->
                                Just <| CSSBasicTypes.computedCSSColor color

                            _ ->
                                Nothing

                    _ ->
                        Nothing
    in
        [ renderBackground dimensions backgroundColor ] ++ (List.concatMap renderLayoutBox children)


renderBackground : BoxModel.BoxModel -> CSSBasicTypes.RGBAColor -> DisplayCommand
renderBackground boxModel color =
    SolidColor (BoxModel.content boxModel) color
