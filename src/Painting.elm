module Painting exposing (..)

import BoxModel
import CSSBasicTypes
import CSSOM
import Color exposing (..)
import Layout exposing (..)
import Line
import Style exposing (..)


type DisplayCommand
    = SolidColor BoxModel.Rect CSSBasicTypes.RGBAColor


buildDisplayList : LayoutRoot -> List DisplayCommand
buildDisplayList (LayoutRoot { boxModel, styles } children) =
    let
        backgroundColor =
            Maybe.withDefault
                { red = 0, green = 0, blue = 0, alpha = 0 }
            <|
                Just <|
                    CSSOM.usedBackgroundColor styles.backgroundColor
    in
    [ renderBackground boxModel backgroundColor ]
        ++ renderBorders styles boxModel
        ++ List.concatMap renderLayoutBox children


renderLayoutBox : LayoutBox -> List DisplayCommand
renderLayoutBox layoutBox =
    case layoutBox of
        BlockBox { boxModel, styles } children ->
            let
                backgroundColor =
                    Maybe.withDefault
                        { red = 0, green = 0, blue = 0, alpha = 0 }
                    <|
                        Just <|
                            CSSOM.usedBackgroundColor styles.backgroundColor
            in
            [ renderBackground boxModel backgroundColor ]
                ++ renderBorders styles boxModel
                ++ List.concatMap renderLayoutBox children

        BlockBoxInlineContext { boxModel, styles } lineRoots ->
            let
                backgroundColor =
                    Maybe.withDefault
                        { red = 0, green = 0, blue = 0, alpha = 0 }
                    <|
                        Just <|
                            CSSOM.usedBackgroundColor styles.backgroundColor
            in
            [ renderBackground boxModel backgroundColor ]
                ++ renderBorders styles
                    boxModel
                ++ List.concatMap
                    renderLine
                    lineRoots


renderLine : Line.StackedLayoutLineBoxRoot -> List DisplayCommand
renderLine (Line.StackedLayoutLineBoxRoot { x, y } (Line.LayoutLineBoxRoot { width, height } _)) =
    [ SolidColor { x = x, y = y, width = width, height = height } { green = 255, blue = 255, red = 0, alpha = 1.0 } ]


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
        { x = borderBox.x
        , y = borderBox.y
        , width = borderBox.width
        , height = border.top
        }
        topColor
    , SolidColor
        { x = borderBox.x + borderBox.width - border.right
        , y = borderBox.y
        , width = border.right
        , height = borderBox.height
        }
        rightColor
    , SolidColor
        { x = borderBox.x
        , y = borderBox.y + borderBox.height - border.bottom
        , width = borderBox.width
        , height = border.bottom
        }
        bottomColor
    , SolidColor
        { x = borderBox.x
        , y = borderBox.y
        , width = border.left
        , height = borderBox.height
        }
        leftColor
    ]
