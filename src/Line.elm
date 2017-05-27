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


type LayoutLineBoxRoot
    = LayoutLineBoxRoot LayoutLineBoxTree


type LayoutLineBoxTree
    = LayoutLineBoxContainer BoxModel.Rect (List LayoutLineBoxTree)
    | LayoutLineBoxText String BoxModel.Rect


layoutLineBoxRoot : LineBoxRoot -> LayoutLineBoxRoot
layoutLineBoxRoot (LineBoxRoot lineBoxTree) =
    let
        ( width, laidoutChildren, height ) =
            layoutLineBoxTree 0 lineBoxTree
    in
    LayoutLineBoxRoot laidoutChildren


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
    LineBoxRoot (LineBoxContainer (List.map lineBoxTree children))


lineBoxTree : Box.InlineLevelElement -> LineBoxTree
lineBoxTree inlineLevelElement =
    case inlineLevelElement of
        Box.InlineText text ->
            LineBoxText text { width = 50, height = 10 }

        Box.InlineContainer styles children ->
            LineBoxContainer (List.map lineBoxTree children)


getLines :
    LineBoxRoot
    -> Float
    -> List LineBoxRoot
getLines (LineBoxRoot root) maxWidth =
    case getLine (EmptyLine maxWidth) root of
        ( Used child, _ ) ->
            [ LineBoxRoot child ]

        ( Unused child, _ ) ->
            [ LineBoxRoot child ]

        ( Mixed usedChild unusedChild, _ ) ->
            [ LineBoxRoot usedChild ] ++ getLines (LineBoxRoot unusedChild) maxWidth


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
