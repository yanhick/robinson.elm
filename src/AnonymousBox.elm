module AnonymousBox exposing (..)

import Style exposing (..)
import BoxModel exposing (..)
import CSSOM


{- TODO, should expose final layout boxes (block container, block container
   establishing formatting context, block container establishing inlilne inline formatting context,
   inline container, text container)

   block container establishing inline context and inline container should only be allowed
   inline container children and text container children

   later: add in-flow and out-of-flow (fixed , absolute and floater box)box generation
   generated content also? and table?

   call it normalized tree? or box generated tree? Box tree? and layout tree?

   all those tree should have a root, then root children, no maybe?

-}


type InlineLevelElement
    = InlineContainer Styles (List InlineLevelElement)
    | InlineText String


type BlockLevelElement
    = BlockContainer Styles (List Box)


type Box
    = BlockLevel BlockLevelElement
    | InlineLevel InlineLevelElement


type BoxRoot
    = BoxRoot Styles (List Box)


type IntermediateBox
    = IntermediateInlineContainer Styles (List IntermediateBox)
    | IntermediateInlineText String
    | IntermediateAnonymousInlineRoot (List IntermediateBox)
    | IntermediateBlockContainer Styles (List IntermediateBox)
    | IntermediateAnonymousBlock (List IntermediateBox)


boxTree : StyledRoot -> Maybe BoxRoot
boxTree (StyledRoot { node, styles, children }) =
    case styles.display of
        CSSOM.None ->
            Nothing

        _ ->
            Just <|
                BoxRoot styles <|
                    List.filterMap (boxTreeFinalStep Style.initialStyles)
                        (List.filterMap intermediateBoxTree children)


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
                            (IntermediateBlockContainer
                                styles
                                (fixAnonymousChildrenForBlockContainer <|
                                    intermediateBoxChildren children
                                )
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


boxTreeFinalStep : Styles -> IntermediateBox -> Maybe Box
boxTreeFinalStep parentStyles intermediateBox =
    let
        anoChildren styles =
            List.filterMap (boxTreeFinalStep styles)

        flattenChildren styles children =
            List.foldl
                (\child children ->
                    case child of
                        IntermediateAnonymousInlineRoot list ->
                            List.append children (anoChildren styles list)

                        _ ->
                            List.append children (anoChildren styles [ child ])
                )
                []
                children

        getInlineChildren children =
            List.filterMap
                (\child ->
                    case child of
                        IntermediateInlineContainer styles list ->
                            Just <|
                                InlineContainer styles
                                    (getInlineChildren list)

                        IntermediateInlineText text ->
                            Just <| InlineText text

                        _ ->
                            Nothing
                )
                children
    in
        case intermediateBox of
            IntermediateBlockContainer styles children ->
                Just <| BlockLevel <| BlockContainer styles (flattenChildren styles children)

            IntermediateAnonymousBlock children ->
                Just <|
                    BlockLevel <|
                        BlockContainer parentStyles (flattenChildren parentStyles children)

            IntermediateInlineContainer styles children ->
                Just <|
                    InlineLevel <|
                        InlineContainer styles
                            (getInlineChildren children)

            IntermediateInlineText text ->
                Just <| InlineLevel <| InlineText text

            IntermediateAnonymousInlineRoot children ->
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
