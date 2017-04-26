module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)
import DOM exposing (..)


type Box
    = Block StyledNode
    | Inline StyledNode
    | Anonymous


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


dimensions : Rect -> EdgeSize -> EdgeSize -> EdgeSize -> Dimensions
dimensions content padding border margin =
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


calculateBlockHeight : StyledElementNode -> Float
calculateBlockHeight node =
    case node.styles.height of
        Style.Length l _ ->
            l

        _ ->
            0.0
