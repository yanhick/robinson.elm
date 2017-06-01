module BoxModel
    exposing
        ( BoxModel
        , EdgeSize
        , Rect
        , blockHeight
        , blockPosition
        , blockWidth
        , border
        , borderBox
        , content
        , default
        , make
        , margin
        , marginBox
        , padding
        , paddingBox
        )

import CSSBasicTypes
import CSSOM
import Style


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


type BoxModel
    = BoxModel Dimensions


type alias Dimensions =
    { content : Rect
    , padding : EdgeSize
    , border : EdgeSize
    , margin : EdgeSize
    }


padding : BoxModel -> EdgeSize
padding (BoxModel { padding }) =
    padding


margin : BoxModel -> EdgeSize
margin (BoxModel { margin }) =
    margin


border : BoxModel -> EdgeSize
border (BoxModel { border }) =
    border


content : BoxModel -> Rect
content (BoxModel { content }) =
    content


paddingBox : BoxModel -> Rect
paddingBox (BoxModel { content, padding }) =
    expandRectBy content padding


borderBox : BoxModel -> Rect
borderBox boxModel =
    expandRectBy (paddingBox boxModel) (border boxModel)


marginBox : BoxModel -> Rect
marginBox boxModel =
    expandRectBy (borderBox boxModel) (margin boxModel)


type alias Padding =
    EdgeSize


type alias Border =
    EdgeSize


type alias Margin =
    EdgeSize


make : Rect -> Padding -> Border -> Margin -> BoxModel
make content padding border margin =
    BoxModel
        { content = content
        , padding = padding
        , border = border
        , margin = margin
        }


default : BoxModel
default =
    make
        { x = 0, y = 0, width = 0, height = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }


blockPosition :
    Style.Styles
    -> BoxModel
    -> BoxModel
    -> BoxModel
blockPosition styles boxModel containingBoxModel =
    let
        boxModelPadding =
            padding boxModel

        newPadding =
            { left = boxModelPadding.left
            , right = boxModelPadding.right
            , top = CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingTop
            , bottom = CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingBottom
            }

        boxModelBorder =
            border boxModel

        newBorder =
            { left = boxModelBorder.left
            , right = boxModelBorder.right
            , top =
                styles.borderTopWidth
                    |> CSSOM.computedBorderWidth styles.borderTopStyle
                    |> CSSOM.usedBorderWidth
            , bottom =
                styles.borderBottomWidth
                    |> CSSOM.computedBorderWidth styles.borderTopStyle
                    |> CSSOM.usedBorderWidth
            }

        boxModelMargin =
            margin boxModel

        newMargin =
            { left = boxModelMargin.left
            , right = boxModelMargin.right
            , top = Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin styles.marginTop
            , bottom = Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin styles.marginBottom
            }

        containingBoxModelContent =
            content containingBoxModel

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
            content boxModel

        newContent =
            { width = boxModelContent.width
            , height = boxModelContent.height
            , x = x
            , y = y
            }
    in
    make newContent newPadding newBorder newMargin


blockHeight :
    Style.Styles
    -> BoxModel
    -> BoxModel
blockHeight styles boxModel =
    let
        boxModelContent =
            content boxModel

        newContent =
            { x = boxModelContent.x
            , y = boxModelContent.y
            , width = boxModelContent.width
            , height =
                Maybe.withDefault
                    boxModelContent.height
                <|
                    CSSOM.usedHeight <|
                        CSSOM.computedHeight styles.height
            }
    in
    make
        newContent
        (padding boxModel)
        (border boxModel)
        (margin boxModel)


blockWidth :
    Style.Styles
    -> BoxModel
    -> BoxModel
    -> BoxModel
blockWidth styles boxModel containingBoxModel =
    let
        marginLength l =
            CSSOM.marginLength <| Maybe.withDefault CSSBasicTypes.defaultCSSLength (CSSBasicTypes.cssPixelLength l)

        widthLength l =
            CSSOM.widthLength <| Maybe.withDefault CSSBasicTypes.defaultCSSLength (CSSBasicTypes.cssPixelLength l)

        dimensions =
            [ Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin styles.marginLeft
            , Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin styles.marginRight
            , CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingLeft
            , CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingRight
            , CSSOM.usedBorderWidth <| CSSOM.computedBorderWidth styles.borderLeftStyle styles.borderLeftWidth
            , CSSOM.usedBorderWidth <| CSSOM.computedBorderWidth styles.borderRightStyle styles.borderRightWidth
            , Maybe.withDefault 0 <| CSSOM.usedWidth <| CSSOM.computedWidth styles.width
            ]

        total =
            List.sum dimensions

        containingBoxModelContent =
            content containingBoxModel

        ( constrainedMarginLeft, constrainedMarginRight ) =
            if CSSOM.isAutoWidth styles.width && total > containingBoxModelContent.width then
                let
                    marginLeft =
                        if CSSOM.isAutoMargin styles.marginLeft then
                            CSSOM.defaultMargin
                        else
                            styles.marginLeft

                    marginRight =
                        if CSSOM.isAutoMargin styles.marginRight then
                            CSSOM.defaultMargin
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
            CSSOM.isAutoWidth styles.width

        marginLeftIsAuto =
            CSSOM.isAutoMargin styles.marginLeft

        marginRightIsAuto =
            CSSOM.isAutoMargin styles.marginRight

        ( l, r, w ) =
            if not widthIsAuto && not marginLeftIsAuto && not marginRightIsAuto then
                ( constrainedMarginLeft
                , marginLength <|
                    (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin constrainedMarginRight)
                        + underflow
                , width
                )
            else if not widthIsAuto && not marginLeftIsAuto && marginRightIsAuto then
                ( constrainedMarginLeft
                , marginLength underflow
                , width
                )
            else if not widthIsAuto && marginLeftIsAuto && not marginRightIsAuto then
                ( marginLength underflow
                , constrainedMarginRight
                , width
                )
            else if widthIsAuto then
                if marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0, constrainedMarginRight, widthLength underflow )
                    else
                        ( marginLength 0.0
                        , marginLength <|
                            (Maybe.withDefault 0 <|
                                CSSOM.usedMargin <|
                                    CSSOM.computedMargin constrainedMarginRight
                            )
                                + underflow
                        , widthLength 0.0
                        )
                else if marginRightIsAuto && not marginLeftIsAuto then
                    if underflow >= 0.0 then
                        ( constrainedMarginLeft, marginLength 0.0, widthLength underflow )
                    else
                        ( constrainedMarginLeft, marginLength underflow, widthLength 0.0 )
                else if marginLeftIsAuto && marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( marginLength 0.0, marginLength 0.0, widthLength underflow )
                    else
                        ( marginLength 0.0, marginLength underflow, widthLength 0.0 )
                else if not marginLeftIsAuto && not marginRightIsAuto then
                    if underflow >= 0.0 then
                        ( constrainedMarginLeft, constrainedMarginRight, widthLength underflow )
                    else
                        ( constrainedMarginLeft
                        , marginLength <|
                            (Maybe.withDefault 0 <|
                                CSSOM.usedMargin <|
                                    CSSOM.computedMargin constrainedMarginRight
                            )
                                + underflow
                        , widthLength 0.0
                        )
                else
                    ( constrainedMarginLeft
                    , constrainedMarginRight
                    , width
                    )
            else if not widthIsAuto && marginLeftIsAuto && marginRightIsAuto then
                ( marginLength (underflow / 2.0)
                , marginLength (underflow / 2.0)
                , width
                )
            else
                ( constrainedMarginLeft
                , constrainedMarginRight
                , width
                )

        oldContent =
            content boxModel

        newContent =
            { oldContent | width = Maybe.withDefault 0 <| CSSOM.usedWidth <| CSSOM.computedWidth w }

        oldMargin =
            margin boxModel

        newMargin =
            { oldMargin
                | left = Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin l
                , right = Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin r
            }

        oldPadding =
            padding boxModel

        newPadding =
            { oldPadding
                | left = CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingLeft
                , right = CSSOM.usedPadding <| CSSOM.computedPadding styles.paddingRight
            }

        oldBorder =
            border boxModel

        newBorder =
            { oldBorder
                | left = CSSOM.usedBorderWidth <| CSSOM.computedBorderWidth styles.borderLeftStyle styles.borderLeftWidth
                , right = CSSOM.usedBorderWidth <| CSSOM.computedBorderWidth styles.borderRightStyle styles.borderRightWidth
            }
    in
    make
        newContent
        newPadding
        newBorder
        newMargin
