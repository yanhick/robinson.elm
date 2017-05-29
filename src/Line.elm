module Line exposing (..)

import Box
import BoxModel


type LineBoxRoot
    = LineBoxRoot LineBoxTree


type LineBoxTree
    = LineBoxContainer (List LineBoxTree)
    | LineBoxText String { width : Float, height : Float }


type Line
    = EmptyLine Float
    | UsedLine { width : Float, maxWidth : Float }
    | FullLine


type LineBoxNode
    = Used LineBoxTree
    | Unused LineBoxTree
    | Mixed LineBoxTree LineBoxTree


type StackedLayoutLineBoxRoot
    = StackedLayoutLineBoxRoot { x : Float, y : Float } LayoutLineBoxRoot


type LayoutLineBoxRoot
    = LayoutLineBoxRoot { width : Float, height : Float } LayoutLineBoxTree


type LayoutLineBoxTree
    = LayoutLineBoxContainer BoxModel.Rect (List LayoutLineBoxTree)
    | LayoutLineBoxText String BoxModel.Rect


layoutInlineFormattingContext : Box.InlineBoxRoot -> BoxModel.Rect -> List StackedLayoutLineBoxRoot
layoutInlineFormattingContext inlineBoxRoot containingBlockRect =
    inlineBoxRoot
        |> lineBoxRoot
        |> getLines containingBlockRect.width
        |> List.map layoutLineBoxRoot
        |> stackLineBoxes { x = containingBlockRect.x, y = containingBlockRect.y }


stackLineBoxes : { x : Float, y : Float } -> List LayoutLineBoxRoot -> List StackedLayoutLineBoxRoot
stackLineBoxes position layoutLineBoxRoots =
    Tuple.second <|
        List.foldl
            (\(LayoutLineBoxRoot { width, height } layoutLineBoxTree) ( { x, y }, stackedLineBoxes ) ->
                let
                    newPosition =
                        { x = x
                        , y = y + height
                        }
                in
                ( newPosition
                , List.append stackedLineBoxes
                    [ StackedLayoutLineBoxRoot { x = x, y = y }
                        (LayoutLineBoxRoot { width = width, height = height } layoutLineBoxTree)
                    ]
                )
            )
            ( position, [] )
            layoutLineBoxRoots


layoutLineBoxRoot : LineBoxRoot -> LayoutLineBoxRoot
layoutLineBoxRoot (LineBoxRoot lineBoxTree) =
    let
        ( width, laidoutChildren, height ) =
            layoutLineBoxTree 0 lineBoxTree
    in
    LayoutLineBoxRoot { width = width, height = height } laidoutChildren


layoutLineBoxTree : Float -> LineBoxTree -> ( Float, LayoutLineBoxTree, Float )
layoutLineBoxTree x lineBoxTree =
    case lineBoxTree of
        LineBoxText text textDimensions ->
            ( textDimensions.width
            , LayoutLineBoxText
                text
                { x = x, y = 0, width = textDimensions.width, height = textDimensions.height }
            , textDimensions.height
            )

        LineBoxContainer children ->
            let
                ( totalWidth, laidoutChildren, childrenMaxHeight ) =
                    List.foldl
                        (\child ( childrenWidth, children, currentMaxHeight ) ->
                            let
                                ( childWidth, laidoutChild, childHeight ) =
                                    layoutLineBoxTree (x + childrenWidth) child

                                maxHeight =
                                    max currentMaxHeight childHeight
                            in
                            ( childWidth + childrenWidth, List.append children [ laidoutChild ], maxHeight )
                        )
                        ( 0, [], 0 )
                        children
            in
            ( totalWidth
            , LayoutLineBoxContainer
                { x = x, y = 0, width = totalWidth, height = childrenMaxHeight }
                laidoutChildren
            , childrenMaxHeight
            )


lineBoxRoot : Box.InlineBoxRoot -> LineBoxRoot
lineBoxRoot (Box.InlineBoxRoot styles children) =
    LineBoxRoot (LineBoxContainer (List.concatMap lineBoxTree children))


lineBoxTree : Box.InlineLevelElement -> List LineBoxTree
lineBoxTree inlineLevelElement =
    case inlineLevelElement of
        Box.InlineText text ->
            List.map
                (\text -> LineBoxText text (measureText text))
                (splitText text)

        Box.InlineContainer styles children ->
            [ LineBoxContainer (List.concatMap lineBoxTree children) ]


splitText : String -> List String
splitText text =
    text
        |> String.split " "
        |> List.intersperse " "


measureText : String -> { width : Float, height : Float }
measureText text =
    { width = toFloat <| 5 * String.length text, height = 10 }


getLines :
    Float
    -> LineBoxRoot
    -> List LineBoxRoot
getLines maxWidth (LineBoxRoot root) =
    case getLine (EmptyLine maxWidth) root of
        ( Used child, _ ) ->
            [ LineBoxRoot child ]

        ( Unused child, _ ) ->
            [ LineBoxRoot child ]

        ( Mixed usedChild unusedChild, _ ) ->
            [ LineBoxRoot usedChild ] ++ getLines maxWidth (LineBoxRoot unusedChild)


getLine :
    Line
    -> LineBoxTree
    -> ( LineBoxNode, Line )
getLine line lineBoxTree =
    case lineBoxTree of
        LineBoxContainer children ->
            let
                res =
                    List.foldl
                        (\child { usedChildren, line, unusedChildren } ->
                            case getLine line child of
                                ( Used usedChild, line ) ->
                                    { usedChildren = List.append usedChildren [ usedChild ]
                                    , unusedChildren = unusedChildren
                                    , line = line
                                    }

                                ( Unused unusedChild, line ) ->
                                    { unusedChildren = List.append unusedChildren [ unusedChild ]
                                    , usedChildren = usedChildren
                                    , line = line
                                    }

                                ( Mixed usedChild unusedChild, line ) ->
                                    { unusedChildren = List.append unusedChildren [ unusedChild ]
                                    , usedChildren = List.append usedChildren [ usedChild ]
                                    , line = line
                                    }
                        )
                        { usedChildren = [], unusedChildren = [], line = line }
                        children
            in
            if List.length res.unusedChildren == 0 then
                ( Used (LineBoxContainer res.usedChildren), res.line )
            else if List.length res.usedChildren == 0 then
                ( Unused (LineBoxContainer res.unusedChildren), res.line )
            else
                ( Mixed (LineBoxContainer res.usedChildren) (LineBoxContainer res.unusedChildren), res.line )

        LineBoxText text textDimensions ->
            case line of
                EmptyLine maxWidth ->
                    if textDimensions.width > maxWidth then
                        ( Used (LineBoxText text textDimensions)
                        , FullLine
                        )
                    else
                        ( Used (LineBoxText text textDimensions)
                        , UsedLine { width = textDimensions.width, maxWidth = maxWidth }
                        )

                UsedLine { width, maxWidth } ->
                    if width + textDimensions.width > maxWidth then
                        ( Unused (LineBoxText text textDimensions)
                        , FullLine
                        )
                    else
                        ( Used (LineBoxText text textDimensions)
                        , UsedLine { width = textDimensions.width + width, maxWidth = maxWidth }
                        )

                FullLine ->
                    ( Unused (LineBoxText text textDimensions)
                    , FullLine
                    )
