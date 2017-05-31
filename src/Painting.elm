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
    | Text BoxModel.Rect String


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
                    renderLineRoot
                    lineRoots


renderLineRoot : Line.StackedLayoutLineBoxRoot -> List DisplayCommand
renderLineRoot (Line.StackedLayoutLineBoxRoot linePosition (Line.LayoutLineBoxRoot { width, height } layoutBox)) =
    renderLineBox linePosition layoutBox


renderLineBox : { x : Float, y : Float } -> Line.LayoutLineBoxTree -> List DisplayCommand
renderLineBox linePosition layoutBox =
    case layoutBox of
        Line.LayoutLineBoxText text { x, y, width, height } ->
            [ Text { x = x + linePosition.x, y = y + linePosition.y, width = width, height = height } text ]

        Line.LayoutLineBoxContainer { x, y, width, height } children ->
            [ SolidColor { x = linePosition.x, y = linePosition.y, width = width, height = height } { green = 255, blue = 255, red = 0, alpha = 1.0 } ]
                ++ List.concatMap (renderLineBox linePosition) children


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
