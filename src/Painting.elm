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


type alias Offset =
    { x : Float, y : Float }


getOffset : Style.Styles -> Offset
getOffset styles =
    CSSOM.usedOffsets styles.position
        { top = styles.top
        , bottom = styles.bottom
        , left = styles.left
        , right = styles.right
        }


addOffsets : Offset -> Offset -> Offset
addOffsets a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


buildDisplayList : Layout.LayoutRoot -> List DisplayCommand
buildDisplayList (Layout.LayoutRoot { boxModel, styles } children) =
    renderBlockBox boxModel styles (getOffset styles) children


renderBlockBox :
    BoxModel.BoxModel
    -> Style.Styles
    -> Offset
    -> List Layout.LayoutBox
    -> List DisplayCommand
renderBlockBox boxModel styles offset children =
    let
        combinedOffset =
            addOffsets offset (getOffset styles)
    in
    renderBackground boxModel (CSSOM.usedBackgroundColor styles.backgroundColor) combinedOffset
        :: renderBorders offset styles boxModel
        ++ List.concatMap (renderLayoutBox combinedOffset) children


renderLayoutBox : Offset -> Layout.LayoutBox -> List DisplayCommand
renderLayoutBox offset layoutBox =
    case layoutBox of
        Layout.BlockBox { boxModel, styles } children ->
            renderBlockBox boxModel styles offset children

        Layout.BlockBoxInlineContext { boxModel, styles } lineRoots ->
            let
                combinedOffset =
                    addOffsets offset (getOffset styles)
            in
            renderBackground boxModel (CSSOM.usedBackgroundColor styles.backgroundColor) combinedOffset
                :: renderBorders combinedOffset
                    styles
                    boxModel
                ++ List.concatMap
                    (renderLineRoot combinedOffset)
                    lineRoots


renderLineRoot : Offset -> Line.StackedLayoutLineBoxRoot -> List DisplayCommand
renderLineRoot offset (Line.StackedLayoutLineBoxRoot linePosition (Line.LayoutLineBoxRoot _ layoutBox)) =
    renderLineBox { x = linePosition.x + offset.x, y = linePosition.y + offset.y } layoutBox


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


renderBackground : BoxModel.BoxModel -> CSSBasicTypes.RGBAColor -> Offset -> DisplayCommand
renderBackground boxModel color offset =
    let
        borderBox =
            BoxModel.borderBox boxModel
    in
    SolidColor
        { x = borderBox.x + offset.x
        , y = borderBox.y + offset.y
        , width = borderBox.width
        , height = borderBox.height
        }
        color


renderBorders : Offset -> Style.Styles -> BoxModel.BoxModel -> List DisplayCommand
renderBorders offset styles boxModel =
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
        { x = borderBox.x + offset.x
        , y = borderBox.y + offset.y
        , width = borderBox.width
        , height = border.top
        }
        topColor
    , SolidColor
        { x = borderBox.x + borderBox.width - border.right + offset.x
        , y = borderBox.y + offset.y
        , width = border.right
        , height = borderBox.height
        }
        rightColor
    , SolidColor
        { x = borderBox.x + offset.x
        , y = borderBox.y + borderBox.height - border.bottom + offset.y
        , width = borderBox.width
        , height = border.bottom
        }
        bottomColor
    , SolidColor
        { x = borderBox.x + offset.x
        , y = borderBox.y + offset.y
        , width = border.left
        , height = borderBox.height
        }
        leftColor
    ]
