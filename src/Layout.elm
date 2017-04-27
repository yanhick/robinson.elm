module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)


type Box
    = Block StyledNode
    | Inline StyledNode
    | Anonymous
    | None


type alias EdgeSize =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


expandRectBy : Rect -> EdgeSize -> Rect
expandRectBy rect edge =
    { x = rect.x - edge.left
    , y = rect.y - edge.top
    , width = rect.width + edge.left + edge.right
    , height = rect.height + edge.top + edge.bottom
    }


type LayoutBox
    = LayoutBox
        { dimensions : Dimensions
        , box : Box
        , children : List LayoutBox
        }


buildDimensions : Rect -> EdgeSize -> EdgeSize -> EdgeSize -> Dimensions
buildDimensions content padding border margin =
    let
        paddingBox =
            expandRectBy content padding

        borderBox =
            expandRectBy paddingBox border

        marginBox =
            expandRectBy borderBox margin
    in
        { content = content
        , padding = padding
        , border = border
        , margin = margin
        , paddingBox = paddingBox
        , borderBox = borderBox
        , marginBox = marginBox
        }


type alias Dimensions =
    { paddingBox : Rect
    , borderBox : Rect
    , marginBox : Rect
    , content : Rect
    , padding : EdgeSize
    , border : EdgeSize
    , margin : EdgeSize
    }


toPx dimension =
    case dimension of
        Style.Length l _ ->
            l

        _ ->
            0.0


startLayout : StyledNode -> Dimensions -> LayoutBox
startLayout node containingBlockDimensions =
    layout (layoutTree node) containingBlockDimensions


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
                        , dimensions = initialDimensions
                        , children = childrenBox children
                        }

                StyledText _ ->
                    { box = Inline node, dimensions = initialDimensions, children = [] }


layout : LayoutBox -> Dimensions -> LayoutBox
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


initialDimensions : Dimensions
initialDimensions =
    buildDimensions
        { x = 0, y = 0, width = 0, height = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }


layoutBlock : LayoutBox -> Dimensions -> LayoutBox
layoutBlock (LayoutBox { box, children, dimensions }) containingBlockDimensions =
    case box of
        Block (StyledElement node) ->
            let
                widthDimensions =
                    calculateBlockWidth node dimensions containingBlockDimensions

                positionedDimensions =
                    calculateBlockPosition node widthDimensions containingBlockDimensions

                ( laidoutChildren, laidoutContainingBlock ) =
                    layoutBlockChildren children positionedDimensions containingBlockDimensions

                content =
                    { x = positionedDimensions.content.x
                    , y = positionedDimensions.content.y
                    , width = widthDimensions.content.width
                    , height = laidoutContainingBlock.content.height
                    }

                laidoutDimensions =
                    calculateBlockHeight node { positionedDimensions | content = content }
            in
                LayoutBox
                    { box = box
                    , children = laidoutChildren
                    , dimensions = laidoutDimensions
                    }

        _ ->
            LayoutBox { box = box, children = children, dimensions = dimensions }


layoutBlockChildren : List LayoutBox -> Dimensions -> Dimensions -> ( List LayoutBox, Dimensions )
layoutBlockChildren children blockDimensions containingBlockDimensions =
    List.foldl
        (\child ( children, containingBlockDimensions ) ->
            let
                childLayoutBox =
                    layout child containingBlockDimensions

                childDimensions (LayoutBox { dimensions }) =
                    dimensions

                content =
                    { x = containingBlockDimensions.content.x
                    , y = containingBlockDimensions.content.y
                    , width = containingBlockDimensions.content.width
                    , height = containingBlockDimensions.content.height + (childDimensions childLayoutBox).content.height
                    }
            in
                ( List.append children [ childLayoutBox ]
                , { containingBlockDimensions
                    | content = content
                  }
                )
        )
        ( [], containingBlockDimensions )
        children


calculateBlockWidth : StyledElementNode -> Dimensions -> Dimensions -> Dimensions
calculateBlockWidth { node, styles } blockDimensions containingBlockDimensions =
    let
        isAuto dimension =
            case dimension of
                Auto ->
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

        ( marginLeft, marginRight ) =
            if isAuto styles.width && total > containingBlockDimensions.content.width then
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
            containingBlockDimensions.content.width - total

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
            blockDimensions.content

        newContent =
            { oldContent | width = toPx w }

        oldMargins =
            blockDimensions.margin

        newMargins =
            { oldMargins | left = toPx l, right = toPx r }
    in
        { blockDimensions | content = newContent, margin = newMargins }


calculateBlockHeight : StyledElementNode -> Dimensions -> Dimensions
calculateBlockHeight { styles } dimensions =
    case styles.height of
        Style.Length length _ ->
            let
                content =
                    { x = dimensions.content.x
                    , y = dimensions.content.y
                    , width = dimensions.content.width
                    , height = length
                    }
            in
                { dimensions | content = content }

        _ ->
            dimensions


calculateBlockPosition : StyledElementNode -> Dimensions -> Dimensions -> Dimensions
calculateBlockPosition { node, styles } blockDimensions containingBlockDimension =
    let
        padding =
            { left = blockDimensions.padding.left
            , right = blockDimensions.padding.right
            , top = toPx styles.paddingTop
            , bottom = toPx styles.paddingBottom
            }

        border =
            { left = blockDimensions.border.left
            , right = blockDimensions.border.right
            , top = toPx styles.borderTop
            , bottom = toPx styles.borderBottom
            }

        margin =
            { left = blockDimensions.margin.left
            , right = blockDimensions.margin.right
            , top = toPx styles.marginTop
            , bottom = toPx styles.marginBottom
            }

        x =
            containingBlockDimension.content.x
                + margin.left
                + border.left
                + padding.left

        y =
            containingBlockDimension.content.y
                + containingBlockDimension.content.height
                + margin.top
                + border.top
                + padding.top

        content =
            { width = blockDimensions.content.width
            , height = blockDimensions.content.height
            , x = x
            , y = y
            }
    in
        { blockDimensions
            | padding = padding
            , border = border
            , margin = margin
            , content = content
        }
