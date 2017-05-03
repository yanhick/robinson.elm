module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)
import BoxModel
import CSSBasicTypes exposing (..)


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
        marginLength l u =
            CSSOM.marginLength <| Maybe.withDefault defaultCSSLength (cssLength l u)

        widthLength l u =
            CSSOM.widthLength <| Maybe.withDefault defaultCSSLength (cssLength l u)

        dimensions =
            [ Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginLeft
            , Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginRight
            , usedPadding <| computedPadding styles.paddingLeft
            , usedPadding <| computedPadding styles.paddingRight
            , usedBorderWidth <| computedBorderWidth styles.borderLeftWidth
            , usedBorderWidth <| computedBorderWidth styles.borderRightWidth
            , Maybe.withDefault 0 <| usedWidth <| computedWidth styles.width
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
                            defaultMargin
                        else
                            styles.marginLeft

                    marginRight =
                        if isAutoMargin styles.marginRight then
                            defaultMargin
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
                , marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow) Pixel
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
                        ( marginLength 0.0 Pixel, marginRight, widthLength underflow Pixel )
                    else
                        ( marginLength 0.0 Pixel, marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow) Pixel, widthLength 0.0 Pixel )
                else if marginRightIsAuto && not marginLeftIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginLength 0.0 Pixel, widthLength underflow Pixel )
                    else
                        ( marginLeft, marginLength (underflow) Pixel, widthLength 0.0 Pixel )
                else if marginLeftIsAuto && marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0 Pixel, marginLength 0.0 Pixel, widthLength underflow Pixel )
                    else
                        ( marginLength 0.0 Pixel, marginLength (underflow) Pixel, widthLength 0.0 Pixel )
                else if not marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginRight, widthLength underflow Pixel )
                    else
                        ( marginLeft, marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow) Pixel, widthLength 0.0 Pixel )
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
            { oldContent | width = Maybe.withDefault 0 <| usedWidth <| computedWidth w }

        oldMargin =
            BoxModel.margin boxModel

        newMargin =
            { oldMargin
                | left = Maybe.withDefault 0 <| usedMargin <| computedMargin l
                , right = Maybe.withDefault 0 <| usedMargin <| computedMargin r
            }

        oldPadding =
            BoxModel.padding boxModel

        newPadding =
            { oldPadding
                | left = usedPadding <| computedPadding styles.paddingLeft
                , right = usedPadding <| computedPadding styles.paddingRight
            }

        oldBorder =
            BoxModel.border boxModel

        newBorder =
            { oldBorder
                | left = usedBorderWidth <| computedBorderWidth styles.borderLeftWidth
                , right = usedBorderWidth <| computedBorderWidth styles.borderRightWidth
            }
    in
        BoxModel.boxModel
            newContent
            newPadding
            newBorder
            newMargin


calculateBlockHeight :
    StyledElementNode
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockHeight { styles } boxModel =
    let
        boxModelContent =
            BoxModel.content boxModel

        newContent =
            { x = boxModelContent.x
            , y = boxModelContent.y
            , width = boxModelContent.width
            , height =
                Maybe.withDefault
                    boxModelContent.height
                <|
                    usedHeight <|
                        computedHeight styles.height
            }
    in
        BoxModel.boxModel
            newContent
            (BoxModel.padding boxModel)
            (BoxModel.border boxModel)
            (BoxModel.margin boxModel)


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
            , top = usedPadding <| computedPadding styles.paddingTop
            , bottom = usedPadding <| computedPadding styles.paddingBottom
            }

        boxModelBorder =
            BoxModel.border boxModel

        newBorder =
            { left = boxModelBorder.left
            , right = boxModelBorder.right
            , top = usedBorderWidth <| computedBorderWidth <| styles.borderTopWidth
            , bottom = usedBorderWidth <| computedBorderWidth <| styles.borderBottomWidth
            }

        boxModelMargin =
            BoxModel.margin boxModel

        newMargin =
            { left = boxModelMargin.left
            , right = boxModelMargin.right
            , top = Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginTop
            , bottom = Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginBottom
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
