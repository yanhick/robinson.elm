module BoxModel
    exposing
        ( BoxModel
        , EdgeSize
        , Rect
        , border
        , borderBox
        , boxModel
        , content
        , initBoxModel
        , margin
        , marginBox
        , padding
        , paddingBox
        )


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


boxModel : Rect -> Padding -> Border -> Margin -> BoxModel
boxModel content padding border margin =
    BoxModel
        { content = content
        , padding = padding
        , border = border
        , margin = margin
        }


initBoxModel : BoxModel
initBoxModel =
    boxModel
        { x = 0, y = 0, width = 0, height = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
        { top = 0, right = 0, bottom = 0, left = 0 }
