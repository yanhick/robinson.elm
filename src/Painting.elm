module Painting exposing (..)

import Style
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

        styles =
            Maybe.withDefault Style.initialStyles <|
                case box of
                    Layout.Block (StyledElement { styles }) ->
                        Just styles

                    _ ->
                        Nothing
    in
        [ renderBackground dimensions backgroundColor ]
            ++ renderBorders styles dimensions
            ++ (List.concatMap renderLayoutBox children)


renderBackground : BoxModel.BoxModel -> CSSBasicTypes.RGBAColor -> DisplayCommand
renderBackground boxModel color =
    SolidColor (BoxModel.borderBox boxModel) color


renderBorders : Styles -> BoxModel.BoxModel -> List DisplayCommand
renderBorders styles boxModel =
    let
        topColor =
            case styles.borderTopColor of
                CSSOM.BorderColorColor color ->
                    CSSBasicTypes.computedCSSColor color

                _ ->
                    { red = 0, green = 0, blue = 0, alpha = 0 }

        borderBox =
            BoxModel.borderBox boxModel

        border =
            BoxModel.border boxModel
    in
        [ SolidColor
            ({ x = borderBox.x
             , y = borderBox.y
             , width = borderBox.width
             , height = border.top
             }
            )
            topColor
        ]
