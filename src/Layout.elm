module Layout exposing (..)

import Style exposing (..)
import CSSOM exposing (..)


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
        , children : LayoutBox
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


calculateBlockWidth : StyledNode -> Dimensions -> Dimensions
calculateBlockWidth (StyledNode node) containingBlock =
    let
        toPx dimension =
            case dimension of
                Style.Length l _ ->
                    l

                _ ->
                    0.0

        isAuto dimension =
            case dimension of
                Auto ->
                    True

                _ ->
                    False

        dimensions =
            [ node.styles.marginLeft
            , node.styles.marginRight
            , node.styles.paddingLeft
            , node.styles.paddingRight
            , node.styles.borderLeft
            , node.styles.borderRight
            , node.styles.width
            ]

        total =
            List.foldl
                (\dimension acc -> acc + toPx dimension)
                0
                dimensions

        ( marginLeft, marginRight ) =
            if isAuto node.styles.width && total > containingBlock.content.width then
                let
                    marginLeft =
                        if isAuto node.styles.marginLeft then
                            Style.Length 0.0 Pixel
                        else
                            node.styles.marginLeft

                    marginRight =
                        if isAuto node.styles.marginRight then
                            Style.Length 0.0 Pixel
                        else
                            node.styles.marginRight
                in
                    ( marginLeft, marginRight )
            else
                ( node.styles.marginLeft, node.styles.marginRight )
    in
        containingBlock


calculateBlockHeight : StyledNode -> Float
calculateBlockHeight (StyledNode node) =
    case node.styles.height of
        Style.Length l _ ->
            l

        _ ->
            0.0
