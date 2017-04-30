module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)
import BoxModel


type Box
    = Block StyledNode
    | Inline StyledNode
    | Anonymous
    | None


type LayoutBox
    = LayoutBox
        { dimensions : BoxModel.BoxModel
        , box : Box
        , children : List LayoutBox
        }


toPx dimension =
    case dimension of
        Style.Length l _ ->
            l

        _ ->
            0.0


startLayout : StyledNode -> BoxModel.BoxModel -> LayoutBox
startLayout node containingBoxModel =
    layout (layoutTree node) containingBoxModel


layoutTree : StyledNode -> LayoutBox
layoutTree node =
    let
        childrenBox =
            List.foldl
                (\child children ->
                    case child of
                        StyledElement { styles } ->
                            case styles.display of
                                Style.Block ->
                                    List.append children <|
                                        List.singleton <|
                                            layoutTree child

                                _ ->
                                    children

                        StyledText _ ->
                            children
                )
                []
    in
        LayoutBox <|
            case node of
                StyledElement { styles, children } ->
                    let
                        box =
                            case styles.display of
                                Style.Block ->
                                    Block node

                                Style.Inline ->
                                    Inline node

                                Style.None ->
                                    None
                    in
                        { box = box
                        , dimensions = BoxModel.initBoxModel
                        , children = childrenBox children
                        }

                StyledText _ ->
                    { box = Inline node, dimensions = BoxModel.initBoxModel, children = [] }


layout : LayoutBox -> BoxModel.BoxModel -> LayoutBox
layout layoutBox containingBlockDimensions =
    let
        (LayoutBox { box, children, dimensions }) =
            layoutBox
    in
        case box of
            Block node ->
                case node of
                    StyledElement element ->
                        layoutBlock layoutBox containingBlockDimensions

                    StyledText _ ->
                        layoutBox

            _ ->
                LayoutBox
                    { box = box
                    , dimensions = dimensions
                    , children = children
                    }


layoutBlock :
    LayoutBox
    -> BoxModel.BoxModel
    -> LayoutBox
layoutBlock (LayoutBox { box, children, dimensions }) containingBoxModel =
    case box of
        Block (StyledElement node) ->
            let
                boxModelWithCorrectWidth =
                    calculateBlockWidth node dimensions containingBoxModel

                boxModelWithCorrectPosition =
                    calculateBlockPosition node boxModelWithCorrectWidth containingBoxModel

                ( laidoutChildren, childrenBoxModel ) =
                    layoutBlockChildren children boxModelWithCorrectPosition containingBoxModel

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
                    BoxModel.boxModel
                        newContent
                        (BoxModel.padding boxModelWithCorrectPosition)
                        (BoxModel.border boxModelWithCorrectPosition)
                        (BoxModel.margin boxModelWithCorrectPosition)

                newBoxModel =
                    calculateBlockHeight node horizontalBoxModel
            in
                LayoutBox
                    { box = box
                    , children = laidoutChildren
                    , dimensions = newBoxModel
                    }

        _ ->
            LayoutBox { box = box, children = children, dimensions = dimensions }


layoutBlockChildren :
    List LayoutBox
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
    -> ( List LayoutBox, BoxModel.BoxModel )
layoutBlockChildren children boxModel containingBoxModel =
    List.foldl
        (\child ( children, containingBoxModel ) ->
            let
                childLayoutBox =
                    layout child containingBoxModel

                childBoxModelContent (LayoutBox { dimensions }) =
                    BoxModel.marginBox dimensions

                containingBoxModelContent =
                    BoxModel.content containingBoxModel

                newContent =
                    { x = containingBoxModelContent.x
                    , y = containingBoxModelContent.y
                    , width = containingBoxModelContent.width
                    , height =
                        containingBoxModelContent.height
                            + (childBoxModelContent childLayoutBox).height
                    }
            in
                ( List.append children [ childLayoutBox ]
                , BoxModel.boxModel
                    newContent
                    (BoxModel.padding boxModel)
                    (BoxModel.border boxModel)
                    (BoxModel.margin boxModel)
                )
        )
        ( [], containingBoxModel )
        children


calculateBlockWidth :
    StyledElementNode
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockWidth { node, styles } boxModel containingBoxModel =
    let
        isAuto dimension =
            case dimension of
                Style.Auto ->
                    True

                _ ->
                    False

        dimensions =
            [ styles.marginLeft
            , styles.marginRight
            , styles.paddingLeft
            , styles.paddingRight
            , styles.borderLeft
            , styles.borderRight
            , styles.width
            ]

        total =
            List.sum <| List.map toPx dimensions

        containingBoxModelContent =
            BoxModel.content containingBoxModel

        ( marginLeft, marginRight ) =
            if isAuto styles.width && total > containingBoxModelContent.width then
                let
                    marginLeft =
                        if isAuto styles.marginLeft then
                            Style.Length 0.0 Pixel
                        else
                            styles.marginLeft

                    marginRight =
                        if isAuto styles.marginRight then
                            Style.Length 0.0 Pixel
                        else
                            styles.marginRight
                in
                    ( marginLeft, marginRight )
            else
                ( styles.marginLeft, styles.marginRight )

        underflow =
            containingBoxModelContent.width - total

        width =
            styles.width

        widthIsAuto =
            isAuto styles.width

        marginLeftIsAuto =
            isAuto styles.marginLeft

        marginRightIsAuto =
            isAuto styles.marginRight

        ( l, r, w ) =
            if not widthIsAuto && not marginLeftIsAuto && not marginRightIsAuto then
                ( marginLeft
                , Style.Length ((toPx marginRight) + underflow) Pixel
                , width
                )
            else if not widthIsAuto && not marginLeftIsAuto && marginRightIsAuto then
                ( marginLeft
                , Style.Length underflow Pixel
                , width
                )
            else if not widthIsAuto && marginLeftIsAuto && not marginRightIsAuto then
                ( Style.Length underflow Pixel
                , marginRight
                , width
                )
            else if widthIsAuto then
                if marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( Style.Length 0.0 Pixel, marginRight, Style.Length underflow Pixel )
                    else
                        ( Style.Length 0.0 Pixel, Style.Length ((toPx marginRight) + underflow) Pixel, Style.Length 0.0 Pixel )
                else if marginRightIsAuto && not marginLeftIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, Style.Length 0.0 Pixel, Style.Length underflow Pixel )
                    else
                        ( marginLeft, Style.Length (underflow) Pixel, Style.Length 0.0 Pixel )
                else if marginLeftIsAuto && marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( Style.Length 0.0 Pixel, Style.Length 0.0 Pixel, Style.Length underflow Pixel )
                    else
                        ( Style.Length 0.0 Pixel, Style.Length (underflow) Pixel, Style.Length 0.0 Pixel )
                else if not marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginRight, Style.Length underflow Pixel )
                    else
                        ( marginLeft, Style.Length ((toPx marginRight) + underflow) Pixel, Style.Length 0.0 Pixel )
                else
                    ( marginLeft
                    , marginRight
                    , width
                    )
            else if not widthIsAuto && marginLeftIsAuto && marginRightIsAuto then
                ( Style.Length (underflow / 2.0) Pixel
                , Style.Length (underflow / 2.0) Pixel
                , width
                )
            else
                ( marginLeft
                , marginRight
                , width
                )

        oldContent =
            BoxModel.content boxModel

        newContent =
            { oldContent | width = toPx w }

        oldMargin =
            BoxModel.margin boxModel

        newMargin =
            { oldMargin | left = toPx l, right = toPx r }
    in
        BoxModel.boxModel
            newContent
            (BoxModel.padding boxModel)
            (BoxModel.border boxModel)
            newMargin


calculateBlockHeight :
    StyledElementNode
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockHeight { styles } boxModel =
    case styles.height of
        Style.Length length _ ->
            let
                boxModelContent =
                    BoxModel.content boxModel

                newContent =
                    { x = boxModelContent.x
                    , y = boxModelContent.y
                    , width = boxModelContent.width
                    , height = length
                    }
            in
                BoxModel.boxModel
                    newContent
                    (BoxModel.padding boxModel)
                    (BoxModel.border boxModel)
                    (BoxModel.margin boxModel)

        _ ->
            boxModel


calculateBlockPosition :
    StyledElementNode
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockPosition { node, styles } boxModel containingBoxModel =
    let
        boxModelPadding =
            BoxModel.padding boxModel

        newPadding =
            { left = boxModelPadding.left
            , right = boxModelPadding.right
            , top = toPx styles.paddingTop
            , bottom = toPx styles.paddingBottom
            }

        boxModelBorder =
            BoxModel.border boxModel

        newBorder =
            { left = boxModelBorder.left
            , right = boxModelBorder.right
            , top = toPx styles.borderTop
            , bottom = toPx styles.borderBottom
            }

        boxModelMargin =
            BoxModel.margin boxModel

        newMargin =
            { left = boxModelMargin.left
            , right = boxModelMargin.right
            , top = toPx styles.marginTop
            , bottom = toPx styles.marginBottom
            }

        containingBoxModelContent =
            BoxModel.content containingBoxModel

        x =
            containingBoxModelContent.x
                + newMargin.left
                + newBorder.left
                + newPadding.left

        y =
            containingBoxModelContent.y
                + containingBoxModelContent.height
                + newMargin.top
                + newBorder.top
                + newPadding.top

        boxModelContent =
            BoxModel.content boxModel

        newContent =
            { width = boxModelContent.width
            , height = boxModelContent.height
            , x = x
            , y = y
            }
    in
        BoxModel.boxModel newContent newPadding newBorder newMargin
