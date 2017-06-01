module Layout exposing (..)

import Box
import BoxModel
import CSSBasicTypes exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)
import Line
import Style exposing (..)


type alias Box =
    { boxModel : BoxModel.BoxModel
    , styles : Styles
    }


type LayoutRoot
    = LayoutRoot Box (List LayoutBox)


type LayoutBox
    = BlockBox Box (List LayoutBox)
    | BlockBoxInlineContext Box (List Line.StackedLayoutLineBoxRoot)


startLayout : Box.BoxRoot -> BoxModel.BoxModel -> LayoutRoot
startLayout (Box.BoxRoot styles children) containingBoxModel =
    let
        ( box, laidoutChildren ) =
            layoutBlockBox styles children containingBoxModel
    in
    LayoutRoot box laidoutChildren


layoutBlock : Box.BlockLevelElement -> BoxModel.BoxModel -> LayoutBox
layoutBlock blockLevelElement containingBlockDimensions =
    case blockLevelElement of
        Box.BlockContainerBlockContext styles children ->
            let
                ( box, laidoutChildren ) =
                    layoutBlockBox styles children containingBlockDimensions
            in
            BlockBox box laidoutChildren

        Box.BlockContainerInlineContext inlineBoxRoot ->
            let
                ( box, lineBoxes ) =
                    layoutBlockInlineFormattingContext inlineBoxRoot containingBlockDimensions
            in
            BlockBoxInlineContext box lineBoxes


layoutBlockInlineFormattingContext :
    Box.InlineBoxRoot
    -> BoxModel.BoxModel
    -> ( Box, List Line.StackedLayoutLineBoxRoot )
layoutBlockInlineFormattingContext (Box.InlineBoxRoot styles children) containingBoxModel =
    let
        boxModelWithCorrectWidth =
            BoxModel.blockWidth styles BoxModel.default containingBoxModel

        boxModelWithCorrectPosition =
            BoxModel.blockPosition styles boxModelWithCorrectWidth containingBoxModel

        lineBoxes =
            Line.layoutInlineFormattingContext (Box.InlineBoxRoot styles children) (BoxModel.content boxModelWithCorrectPosition)

        boxModelContent =
            BoxModel.content boxModelWithCorrectPosition

        childrenHeight =
            List.foldl
                (\(Line.StackedLayoutLineBoxRoot _ (Line.LayoutLineBoxRoot { height } _)) childrenHeight ->
                    childrenHeight + height
                )
                0
                lineBoxes

        newContent =
            { x = boxModelContent.x
            , y = boxModelContent.y
            , width = boxModelContent.width
            , height = childrenHeight
            }

        horizontalBoxModel =
            BoxModel.make
                newContent
                (BoxModel.padding boxModelWithCorrectPosition)
                (BoxModel.border boxModelWithCorrectPosition)
                (BoxModel.margin boxModelWithCorrectPosition)

        newBoxModel =
            BoxModel.blockHeight styles horizontalBoxModel
    in
    ( { boxModel = newBoxModel, styles = styles }, lineBoxes )


layoutBlockBox :
    Styles
    -> List Box.BlockLevelElement
    -> BoxModel.BoxModel
    -> ( Box, List LayoutBox )
layoutBlockBox styles children containingBoxModel =
    let
        boxModelWithCorrectWidth =
            BoxModel.blockWidth styles BoxModel.default containingBoxModel

        boxModelWithCorrectPosition =
            BoxModel.blockPosition styles boxModelWithCorrectWidth containingBoxModel

        ( laidoutChildren, childrenBoxModel ) =
            layoutBlockChildren children boxModelWithCorrectPosition

        boxModelContent =
            BoxModel.content boxModelWithCorrectPosition

        childrenBoxModelContent =
            BoxModel.content childrenBoxModel

        newContent =
            { x = boxModelContent.x
            , y = boxModelContent.y
            , width = boxModelContent.width
            , height = childrenBoxModelContent.height
            }

        horizontalBoxModel =
            BoxModel.make
                newContent
                (BoxModel.padding boxModelWithCorrectPosition)
                (BoxModel.border boxModelWithCorrectPosition)
                (BoxModel.margin boxModelWithCorrectPosition)

        newBoxModel =
            BoxModel.blockHeight styles horizontalBoxModel
    in
    ( { boxModel = newBoxModel
      , styles = styles
      }
    , laidoutChildren
    )


layoutBlockChildren :
    List Box.BlockLevelElement
    -> BoxModel.BoxModel
    -> ( List LayoutBox, BoxModel.BoxModel )
layoutBlockChildren children containingBoxModel =
    List.foldl
        (\child ( children, containingBoxModel ) ->
            let
                childLayoutBox =
                    layoutBlock child containingBoxModel

                childBoxModelMargin boxModel =
                    BoxModel.marginBox boxModel

                containingBoxModelContent =
                    BoxModel.content containingBoxModel

                newContent boxModel =
                    { x = containingBoxModelContent.x
                    , y = containingBoxModelContent.y
                    , width = containingBoxModelContent.width
                    , height =
                        containingBoxModelContent.height
                            + (childBoxModelMargin boxModel).height
                    }

                addChild boxModel =
                    ( List.append children [ childLayoutBox ]
                    , BoxModel.make
                        (newContent boxModel)
                        (BoxModel.padding boxModel)
                        (BoxModel.border boxModel)
                        (BoxModel.margin boxModel)
                    )
            in
            case childLayoutBox of
                BlockBox { boxModel } _ ->
                    addChild boxModel

                BlockBoxInlineContext { boxModel } _ ->
                    addChild boxModel
        )
        ( [], containingBoxModel )
        children
