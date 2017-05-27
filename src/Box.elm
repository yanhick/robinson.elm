module Box exposing (..)

import BoxModel exposing (..)
import CSSOM
import Style exposing (..)


type InlineLevelElement
    = InlineContainer Styles (List InlineLevelElement)
    | InlineText String


type BlockLevelElement
    = BlockContainerBlockContext Styles (List BlockLevelElement)
    | BlockContainerInlineContext InlineBoxRoot


type InlineBoxRoot
    = InlineBoxRoot Styles (List InlineLevelElement)


type BoxRoot
    = BoxRoot Styles (List BlockLevelElement)


type IntermediateBox
    = IntermediateInlineContainer Styles (List IntermediateBox)
    | IntermediateInlineText String
    | IntermediateAnonymousInlineRoot (List IntermediateBox)
    | IntermediateBlockContainer Styles (List IntermediateBox)
    | IntermediateAnonymousBlock (List IntermediateBox)


type FlattenedBox
    = FlattenedInlineContainer Styles (List FlattenedBox)
    | FlattenedInlineText String
    | FlattenedBlockContainer Styles (List FlattenedBox)
    | FlattenedAnonymousBlock (List FlattenedBox)


boxTree : StyledRoot -> BoxRoot
boxTree (StyledRoot { node, styles, children }) =
    case styles.display of
        CSSOM.None ->
            BoxRoot styles []

        _ ->
            children
                |> List.filterMap intermediateBoxTree
                |> List.filterMap flattenBoxTree
                |> List.filterMap (boxTreeFinalStep styles)
                |> BoxRoot styles


intermediateBoxTree : StyledNode -> Maybe IntermediateBox
intermediateBoxTree node =
    let
        intermediateBoxChildren =
            List.filterMap intermediateBoxTree
    in
    case node of
        StyledElement { styles, children } ->
            case styles.display of
                CSSOM.Block ->
                    Just <|
                        IntermediateBlockContainer
                            styles
                            (fixAnonymousChildrenForBlockContainer <|
                                intermediateBoxChildren children
                            )

                CSSOM.Inline ->
                    Just <|
                        let
                            laidoutChildren =
                                intermediateBoxChildren children

                            anonymousInlineBox =
                                fixAnonymousChildrenForInlineContainer
                                    styles
                                    laidoutChildren
                        in
                        case anonymousInlineBox of
                            Nothing ->
                                IntermediateInlineContainer styles laidoutChildren

                            Just wrappedChildren ->
                                IntermediateAnonymousInlineRoot wrappedChildren

                CSSOM.None ->
                    Nothing

        StyledText text ->
            Just <| IntermediateInlineText text


flattenBoxTree : IntermediateBox -> Maybe FlattenedBox
flattenBoxTree intermediateBox =
    let
        flattenChildren children =
            List.foldl
                (\child children ->
                    case child of
                        IntermediateAnonymousInlineRoot list ->
                            List.append children list

                        _ ->
                            List.append children [ child ]
                )
                []
                children
    in
    case intermediateBox of
        IntermediateBlockContainer styles children ->
            Just <| FlattenedBlockContainer styles (List.filterMap flattenBoxTree <| flattenChildren children)

        IntermediateAnonymousBlock children ->
            Just <| FlattenedAnonymousBlock (List.filterMap flattenBoxTree <| flattenChildren children)

        IntermediateInlineContainer styles children ->
            Just <| FlattenedInlineContainer styles (List.filterMap flattenBoxTree <| flattenChildren children)

        IntermediateInlineText text ->
            Just <| FlattenedInlineText text

        IntermediateAnonymousInlineRoot children ->
            Nothing


boxTreeFinalStep : Styles -> FlattenedBox -> Maybe BlockLevelElement
boxTreeFinalStep parentStyles flattenedBox =
    let
        getInlineChildren children =
            List.filterMap
                (\child ->
                    case child of
                        FlattenedInlineContainer styles list ->
                            Just <|
                                InlineContainer styles
                                    (getInlineChildren list)

                        FlattenedInlineText text ->
                            Just <| InlineText text

                        _ ->
                            Nothing
                )
                children

        allInlineChildren children =
            case children of
                [] ->
                    False

                _ ->
                    List.all isInline children

        isInline child =
            case child of
                FlattenedInlineContainer _ _ ->
                    True

                FlattenedInlineText _ ->
                    True

                _ ->
                    False
    in
    case flattenedBox of
        FlattenedBlockContainer styles children ->
            Just <|
                if allInlineChildren children then
                    BlockContainerInlineContext (InlineBoxRoot styles (getInlineChildren children))
                else
                    BlockContainerBlockContext styles (List.filterMap (boxTreeFinalStep styles) children)

        FlattenedAnonymousBlock children ->
            Just <|
                if allInlineChildren children then
                    BlockContainerInlineContext (InlineBoxRoot parentStyles (getInlineChildren children))
                else
                    BlockContainerBlockContext parentStyles (List.filterMap (boxTreeFinalStep parentStyles) children)

        FlattenedInlineContainer styles children ->
            Nothing

        FlattenedInlineText text ->
            Nothing


allBlockChildren : List IntermediateBox -> Bool
allBlockChildren =
    List.all (not << isInline)


allInlineChildren : List IntermediateBox -> Bool
allInlineChildren =
    List.all isInline


fixAnonymousChildrenForInlineContainer : Styles -> List IntermediateBox -> Maybe (List IntermediateBox)
fixAnonymousChildrenForInlineContainer styles children =
    if allInlineChildren children then
        Nothing
    else
        Just
            (wrapInlineBoxInAnonymousBlockForInlineContainer styles children)


fixAnonymousChildrenForBlockContainer : List IntermediateBox -> List IntermediateBox
fixAnonymousChildrenForBlockContainer children =
    if allBlockChildren children || allInlineChildren children then
        children
    else
        wrapInlineBoxInAnonymousBlockForBlockContainer children


isInline : IntermediateBox -> Bool
isInline child =
    case child of
        IntermediateInlineContainer _ _ ->
            True

        IntermediateInlineText _ ->
            True

        IntermediateAnonymousInlineRoot _ ->
            True

        _ ->
            False


wrapInlineBoxInAnonymousBlockForBlockContainer : List IntermediateBox -> List IntermediateBox
wrapInlineBoxInAnonymousBlockForBlockContainer children =
    let
        wrapInAnonymousBlock children =
            IntermediateAnonymousBlock children

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


wrapInlineBoxInAnonymousBlockForInlineContainer : Styles -> List IntermediateBox -> List IntermediateBox
wrapInlineBoxInAnonymousBlockForInlineContainer styles children =
    wrapInlineBoxInAnonymousBlockForBlockContainer
        ([ IntermediateInlineContainer styles []
         ]
            ++ children
        )
