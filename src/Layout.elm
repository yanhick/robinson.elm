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


marginToPx dimension =
    case dimension of
        MarginLength (CSSLength l _) ->
            l

        MarginAuto ->
            0.0


paddingToPx dimension =
    case dimension of
        Style.Length l _ ->
            l

        _ ->
            0.0


borderToPx dimension =
    case dimension of
        Style.Length l _ ->
            l

        _ ->
            0.0


widthToPx dimension =
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
                                CSSOM.Block ->
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
                                CSSOM.Block ->
                                    Block node

                                CSSOM.Inline ->
                                    Inline node

                                CSSOM.None ->
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

                childBoxModelMargin (LayoutBox { dimensions }) =
                    BoxModel.marginBox dimensions

                containingBoxModelContent =
                    BoxModel.content containingBoxModel

                newContent =
                    { x = containingBoxModelContent.x
                    , y = containingBoxModelContent.y
                    , width = containingBoxModelContent.width
                    , height =
                        containingBoxModelContent.height
                            + (childBoxModelMargin childLayoutBox).height
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
        isAutoWidth dimension =
            case dimension of
                Style.Auto ->
                    True

                _ ->
                    False

        isAutoMargin dimension =
            case dimension of
                MarginAuto ->
                    True

                _ ->
                    False

        marginLength l u =
            MarginLength <| CSSLength l u

        dimensions =
            [ marginToPx styles.marginLeft
            , marginToPx styles.marginRight
            , paddingToPx styles.paddingLeft
            , paddingToPx styles.paddingRight
            , borderToPx styles.borderLeft
            , borderToPx styles.borderRight
            , widthToPx styles.width
            ]

        total =
            List.sum dimensions

        containingBoxModelContent =
            BoxModel.content containingBoxModel

        ( marginLeft, marginRight ) =
            if isAutoWidth styles.width && total > containingBoxModelContent.width then
                let
                    marginLeft =
                        if isAutoMargin styles.marginLeft then
                            MarginLength <| CSSLength 0.0 Pixel
                        else
                            styles.marginLeft

                    marginRight =
                        if isAutoMargin styles.marginRight then
                            MarginLength <| CSSLength 0.0 Pixel
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
            isAutoWidth styles.width

        marginLeftIsAuto =
            isAutoMargin styles.marginLeft

        marginRightIsAuto =
            isAutoMargin styles.marginRight

        ( l, r, w ) =
            if not widthIsAuto && not marginLeftIsAuto && not marginRightIsAuto then
                ( marginLeft
                , marginLength ((marginToPx marginRight) + underflow) Pixel
                , width
                )
            else if not widthIsAuto && not marginLeftIsAuto && marginRightIsAuto then
                ( marginLeft
                , marginLength underflow Pixel
                , width
                )
            else if not widthIsAuto && marginLeftIsAuto && not marginRightIsAuto then
                ( marginLength underflow Pixel
                , marginRight
                , width
                )
            else if widthIsAuto then
                if marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0 Pixel, marginRight, Style.Length underflow Pixel )
                    else
                        ( marginLength 0.0 Pixel, marginLength ((marginToPx marginRight) + underflow) Pixel, Style.Length 0.0 Pixel )
                else if marginRightIsAuto && not marginLeftIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginLength 0.0 Pixel, Style.Length underflow Pixel )
                    else
                        ( marginLeft, marginLength (underflow) Pixel, Style.Length 0.0 Pixel )
                else if marginLeftIsAuto && marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0 Pixel, marginLength 0.0 Pixel, Style.Length underflow Pixel )
                    else
                        ( marginLength 0.0 Pixel, marginLength (underflow) Pixel, Style.Length 0.0 Pixel )
                else if not marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginRight, Style.Length underflow Pixel )
                    else
                        ( marginLeft, marginLength ((marginToPx marginRight) + underflow) Pixel, Style.Length 0.0 Pixel )
                else
                    ( marginLeft
                    , marginRight
                    , width
                    )
            else if not widthIsAuto && marginLeftIsAuto && marginRightIsAuto then
                ( marginLength (underflow / 2.0) Pixel
                , marginLength (underflow / 2.0) Pixel
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
            { oldContent | width = widthToPx w }

        oldMargin =
            BoxModel.margin boxModel

        newMargin =
            { oldMargin | left = marginToPx l, right = marginToPx r }
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
            , top = paddingToPx styles.paddingTop
            , bottom = paddingToPx styles.paddingBottom
            }

        boxModelBorder =
            BoxModel.border boxModel

        newBorder =
            { left = boxModelBorder.left
            , right = boxModelBorder.right
            , top = borderToPx styles.borderTop
            , bottom = borderToPx styles.borderBottom
            }

        boxModelMargin =
            BoxModel.margin boxModel

        newMargin =
            { left = boxModelMargin.left
            , right = boxModelMargin.right
            , top = marginToPx styles.marginTop
            , bottom = marginToPx styles.marginBottom
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
