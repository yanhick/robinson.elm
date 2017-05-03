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
                        Just <| CSSOM.usedBackgroundColor styles.backgroundColor

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
            CSSOM.usedBorderColor styles.borderTopColor

        bottomColor =
            CSSOM.usedBorderColor styles.borderBottomColor

        leftColor =
            CSSOM.usedBorderColor styles.borderLeftColor

        rightColor =
            CSSOM.usedBorderColor styles.borderRightColor

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
        , SolidColor
            ({ x = borderBox.x + borderBox.width - border.right
             , y = borderBox.y
             , width = border.right
             , height = borderBox.height
             }
            )
            rightColor
        , SolidColor
            ({ x = borderBox.x
             , y = borderBox.y + borderBox.height - border.bottom
             , width = borderBox.width
             , height = border.bottom
             }
            )
            bottomColor
        , SolidColor
            ({ x = borderBox.x
             , y = borderBox.y
             , width = border.left
             , height = borderBox.height
             }
            )
            leftColor
        ]
