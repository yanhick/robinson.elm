module Painting exposing (DisplayCommand(..), buildDisplayList)

import BoxModel
import CSSBasicTypes
import CSSOM
import Layout
import Line
import Style


type DisplayCommand
    = SolidColor BoxModel.Rect CSSBasicTypes.RGBAColor
    | Text BoxModel.Rect String


buildDisplayList : Layout.LayoutRoot -> List DisplayCommand
buildDisplayList (Layout.LayoutRoot { boxModel, styles } children) =
    renderBlockBox boxModel styles children


renderBlockBox :
    BoxModel.BoxModel
    -> Style.Styles
    -> List Layout.LayoutBox
    -> List DisplayCommand
renderBlockBox boxModel styles children =
    renderBackground boxModel (CSSOM.usedBackgroundColor styles.backgroundColor)
        :: renderBorders styles boxModel
        ++ List.concatMap renderLayoutBox children


renderLayoutBox : Layout.LayoutBox -> List DisplayCommand
renderLayoutBox layoutBox =
    case layoutBox of
        Layout.BlockBox { boxModel, styles } children ->
            renderBlockBox boxModel styles children

        Layout.BlockBoxInlineContext { boxModel, styles } lineRoots ->
            renderBackground boxModel (CSSOM.usedBackgroundColor styles.backgroundColor)
                :: renderBorders styles
                    boxModel
                ++ List.concatMap
                    renderLineRoot
                    lineRoots


renderLineRoot : Line.StackedLayoutLineBoxRoot -> List DisplayCommand
renderLineRoot (Line.StackedLayoutLineBoxRoot linePosition (Line.LayoutLineBoxRoot _ layoutBox)) =
    renderLineBox linePosition layoutBox


renderLineBox : { x : Float, y : Float } -> Line.LayoutLineBoxTree -> List DisplayCommand
renderLineBox linePosition layoutBox =
    case layoutBox of
        Line.LayoutLineBoxText text { x, y, width, height } ->
            [ Text { x = x + linePosition.x, y = y + linePosition.y, width = width, height = height } text ]

        Line.LayoutLineBoxContainer { width, height } children ->
            SolidColor
                { x = linePosition.x
                , y = linePosition.y
                , width = width
                , height = height
                }
                { green = 255, blue = 255, red = 0, alpha = 1.0 }
                :: List.concatMap (renderLineBox linePosition) children


renderBackground : BoxModel.BoxModel -> CSSBasicTypes.RGBAColor -> DisplayCommand
renderBackground boxModel color =
    SolidColor (BoxModel.borderBox boxModel) color


renderBorders : Style.Styles -> BoxModel.BoxModel -> List DisplayCommand
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
