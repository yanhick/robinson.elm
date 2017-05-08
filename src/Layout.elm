module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)
import BoxModel
import LayoutBox exposing (..)
import CSSBasicTypes exposing (..)


startLayout : StyledNode -> BoxModel.BoxModel -> LayoutBox
startLayout node containingBoxModel =
    layout (layoutTree node) containingBoxModel


layoutTree : StyledNode -> LayoutBox
layoutTree node =
    case node of
        StyledElement { styles, children } ->
            BlockBox
                { styles = styles
                , boxModel = BoxModel.initBoxModel
                , children = layoutTreeChildren children
                }

        StyledText text ->
            TextBox text


layoutTreeChildren : List StyledNode -> List LayoutBox
layoutTreeChildren =
    List.foldl
        (\child children ->
            case child of
                StyledElement { styles } ->
                    case styles.display of
                        Block ->
                            List.append children <|
                                List.singleton <|
                                    layoutTree child

                        Inline ->
                            List.append children <|
                                List.singleton <|
                                    layoutTree child

                        _ ->
                            children

                StyledText _ ->
                    children
        )
        []


allBlockChildren : List LayoutBox -> Bool
allBlockChildren =
    List.all
        (\child ->
            case child of
                BlockBox _ ->
                    True

                _ ->
                    False
        )


allInlineChildren : List LayoutBox -> Bool
allInlineChildren =
    List.all
        (\child ->
            case child of
                InlineBox _ ->
                    True

                _ ->
                    False
        )


fixAnonymousChildrenForInlineContainer : Box -> Box -> Box
fixAnonymousChildrenForInlineContainer inlineContainerBox parentBox =
    if allInlineChildren inlineContainerBox.children then
        inlineContainerBox
    else
        { boxModel = parentBox.boxModel
        , styles = parentBox.styles
        , children =
            parentBox.children
                ++ wrapInlineBoxInAnonymousBlockForInlineContainer inlineContainerBox
        }


fixAnonymousChildrenForBlockContainer : List LayoutBox -> List LayoutBox
fixAnonymousChildrenForBlockContainer children =
    if allBlockChildren children || allInlineChildren children then
        children
    else
        wrapInlineBoxInAnonymousBlockForBlockContainer children


isInline : LayoutBox -> Bool
isInline child =
    case child of
        InlineBox _ ->
            True

        _ ->
            False


wrapInlineBoxInAnonymousBlockForBlockContainer : List LayoutBox -> List LayoutBox
wrapInlineBoxInAnonymousBlockForBlockContainer children =
    let
        wrapInAnonymousBlock children =
            AnonymousBox
                { boxModel =
                    BoxModel.initBoxModel
                , styles = initialStyles
                , children = children
                }

        ( wrappedChildren, remainingInlineChildren ) =
            List.foldl
                (\child ( children, inlineChildren ) ->
                    if isInline child then
                        ( children, inlineChildren ++ [ child ] )
                    else if not (List.isEmpty inlineChildren) then
                        ( children
                            ++ [ wrapInAnonymousBlock inlineChildren ]
                            ++ [ child ]
                        , []
                        )
                    else
                        ( children ++ [ child ], [] )
                )
                ( [], [] )
                children
    in
        if not (List.isEmpty remainingInlineChildren) then
            wrappedChildren
                ++ [ wrapInAnonymousBlock remainingInlineChildren ]
        else
            wrappedChildren


wrapInlineBoxInAnonymousBlockForInlineContainer : Box -> List LayoutBox
wrapInlineBoxInAnonymousBlockForInlineContainer { styles, boxModel, children } =
    wrapInlineBoxInAnonymousBlockForBlockContainer
        ([ InlineBox
            { boxModel = boxModel
            , styles = styles
            , children = []
            }
         ]
            ++ children
        )


layout : LayoutBox -> BoxModel.BoxModel -> LayoutBox
layout layoutBox containingBlockDimensions =
    case layoutBox of
        BlockBox box ->
            BlockBox <| layoutBlock box containingBlockDimensions

        _ ->
            layoutBox


layoutBlock :
    Box
    -> BoxModel.BoxModel
    -> Box
layoutBlock { boxModel, styles, children } containingBoxModel =
    let
        boxModelWithCorrectWidth =
            calculateBlockWidth styles boxModel containingBoxModel

        boxModelWithCorrectPosition =
            calculateBlockPosition styles boxModelWithCorrectWidth containingBoxModel

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
            calculateBlockHeight styles horizontalBoxModel
    in
        { styles = styles
        , children = laidoutChildren
        , boxModel = newBoxModel
        }


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
                    , BoxModel.boxModel
                        (newContent boxModel)
                        (BoxModel.padding boxModel)
                        (BoxModel.border boxModel)
                        (BoxModel.margin boxModel)
                    )
            in
                case childLayoutBox of
                    BlockBox { boxModel } ->
                        addChild boxModel

                    InlineBox { boxModel } ->
                        addChild boxModel

                    AnonymousBox { boxModel } ->
                        addChild boxModel

                    TextBox _ ->
                        ( children, containingBoxModel )
        )
        ( [], containingBoxModel )
        children


calculateBlockWidth :
    Styles
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockWidth styles boxModel containingBoxModel =
    let
        marginLength l =
            CSSOM.marginLength <| Maybe.withDefault defaultCSSLength (cssPixelLength l)

        widthLength l =
            CSSOM.widthLength <| Maybe.withDefault defaultCSSLength (cssPixelLength l)

        dimensions =
            [ Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginLeft
            , Maybe.withDefault 0 <| usedMargin <| computedMargin styles.marginRight
            , usedPadding <| computedPadding styles.paddingLeft
            , usedPadding <| computedPadding styles.paddingRight
            , usedBorderWidth <| computedBorderWidth styles.borderLeftStyle styles.borderLeftWidth
            , usedBorderWidth <| computedBorderWidth styles.borderRightStyle styles.borderRightWidth
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
                , marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow)
                , width
                )
            else if not widthIsAuto && not marginLeftIsAuto && marginRightIsAuto then
                ( marginLeft
                , marginLength underflow
                , width
                )
            else if not widthIsAuto && marginLeftIsAuto && not marginRightIsAuto then
                ( marginLength underflow
                , marginRight
                , width
                )
            else if widthIsAuto then
                if marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0, marginRight, widthLength underflow )
                    else
                        ( marginLength 0.0, marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow), widthLength 0.0 )
                else if marginRightIsAuto && not marginLeftIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginLength 0.0, widthLength underflow )
                    else
                        ( marginLeft, marginLength (underflow), widthLength 0.0 )
                else if marginLeftIsAuto && marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0, marginLength 0.0, widthLength underflow )
                    else
                        ( marginLength 0.0, marginLength (underflow), widthLength 0.0 )
                else if not marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLeft, marginRight, widthLength underflow )
                    else
                        ( marginLeft, marginLength ((Maybe.withDefault 0 <| usedMargin <| computedMargin marginRight) + underflow), widthLength 0.0 )
                else
                    ( marginLeft
                    , marginRight
                    , width
                    )
            else if not widthIsAuto && marginLeftIsAuto && marginRightIsAuto then
                ( marginLength (underflow / 2.0)
                , marginLength (underflow / 2.0)
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
                | left = usedBorderWidth <| computedBorderWidth styles.borderLeftStyle styles.borderLeftWidth
                , right = usedBorderWidth <| computedBorderWidth styles.borderRightStyle styles.borderRightWidth
            }
    in
        BoxModel.boxModel
            newContent
            newPadding
            newBorder
            newMargin


calculateBlockHeight :
    Styles
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockHeight styles boxModel =
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
    Styles
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
    -> BoxModel.BoxModel
calculateBlockPosition styles boxModel containingBoxModel =
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
            , top =
                styles.borderTopWidth
                    |> computedBorderWidth styles.borderTopStyle
                    |> usedBorderWidth
            , bottom =
                styles.borderBottomWidth
                    |> computedBorderWidth styles.borderTopStyle
                    |> usedBorderWidth
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
