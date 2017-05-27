module Line exposing (..)

import Box
import BoxModel


type LineBoxRoot
    = LineBoxRoot LineBoxTree


type LineBoxTree
    = LineBoxContainer (List LineBoxTree)
    | LineBoxText String Float


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
    LayoutLineBoxRoot (Tuple.second (layoutLineBoxTree 0 lineBoxTree))


layoutLineBoxTree : Float -> LineBoxTree -> ( Float, LayoutLineBoxTree )
layoutLineBoxTree x lineBoxTree =
    case lineBoxTree of
        LineBoxText text width ->
            ( width
            , LayoutLineBoxText
                text
                { x = x, y = 0, width = width, height = 0 }
            )

        LineBoxContainer children ->
            let
                ( totalWidth, laidoutChildren ) =
                    List.foldl
                        (\child ( childrenWidth, children ) ->
                            let
                                ( childWidth, laidoutChild ) =
                                    layoutLineBoxTree (x + childrenWidth) child
                            in
                            ( childWidth, List.append children [ laidoutChild ] )
                        )
                        ( 0, [] )
                        children
            in
            ( totalWidth
            , LayoutLineBoxContainer
                { x = 0, y = 0, width = totalWidth, height = 0 }
                laidoutChildren
            )


lineBoxRoot : Box.InlineBoxRoot -> LineBoxRoot
lineBoxRoot (Box.InlineBoxRoot styles children) =
    LineBoxRoot (LineBoxContainer (List.map lineBoxTree children))


lineBoxTree : Box.InlineLevelElement -> LineBoxTree
lineBoxTree inlineLevelElement =
    case inlineLevelElement of
        Box.InlineText text ->
            LineBoxText text 50

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

        LineBoxText text textWidth ->
            case line of
                EmptyLine maxWidth ->
                    if textWidth > maxWidth then
                        ( Used (LineBoxText text textWidth)
                        , FullLine
                        )
                    else
                        ( Used (LineBoxText text textWidth)
                        , UsedLine { width = textWidth, maxWidth = maxWidth }
                        )

                UsedLine { width, maxWidth } ->
                    if width + textWidth > maxWidth then
                        ( Unused (LineBoxText text textWidth)
                        , FullLine
                        )
                    else
                        ( Used (LineBoxText text textWidth)
                        , UsedLine { width = textWidth + width, maxWidth = maxWidth }
                        )

                FullLine ->
                    ( Unused (LineBoxText text textWidth)
                    , FullLine
                    )
