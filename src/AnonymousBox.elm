module AnonymousBox exposing (..)

import Style exposing (..)
import BoxModel exposing (..)
import CSSOM


type InlineLevelElement
    = InlineContainer Styles (List AnonymizedBox)
    | InlineText String
    | AnonymousInlineRoot (List AnonymizedBox)


type BlockLevelElement
    = BlockContainer Styles (List AnonymizedBox)
    | AnonymousBlock (List AnonymizedBox)


type AnonymizedBox
    = BlockLevel BlockLevelElement
    | InlineLevel InlineLevelElement


anonymizedTree : StyledNode -> Maybe AnonymizedBox
anonymizedTree node =
    let
        anonymizedChildren =
            List.filterMap anonymizedTree
    in
        case node of
            StyledElement { styles, children } ->
                case styles.display of
                    CSSOM.Block ->
                        Just <|
                            BlockLevel
                                (BlockContainer
                                    styles
                                    (fixAnonymousChildrenForBlockContainer <|
                                        anonymizedChildren children
                                    )
                                )

                    CSSOM.Inline ->
                        Just <|
                            let
                                laidoutChildren =
                                    anonymizedChildren children

                                anonymousInlineBox =
                                    fixAnonymousChildrenForInlineContainer
                                        styles
                                        laidoutChildren
                            in
                                case anonymousInlineBox of
                                    Nothing ->
                                        InlineLevel <|
                                            InlineContainer styles laidoutChildren

                                    Just wrappedChildren ->
                                        InlineLevel <| AnonymousInlineRoot wrappedChildren

                    CSSOM.None ->
                        Nothing

            StyledText text ->
                Just <| InlineLevel <| InlineText text


allBlockChildren : List AnonymizedBox -> Bool
allBlockChildren =
    List.all (not << isInline)


allInlineChildren : List AnonymizedBox -> Bool
allInlineChildren =
    List.all isInline


fixAnonymousChildrenForInlineContainer : Styles -> List AnonymizedBox -> Maybe (List AnonymizedBox)
fixAnonymousChildrenForInlineContainer styles children =
    if allInlineChildren children then
        Nothing
    else
        Just
            (wrapInlineBoxInAnonymousBlockForInlineContainer styles children)


fixAnonymousChildrenForBlockContainer : List AnonymizedBox -> List AnonymizedBox
fixAnonymousChildrenForBlockContainer children =
    if allBlockChildren children || allInlineChildren children then
        children
    else
        wrapInlineBoxInAnonymousBlockForBlockContainer children


isInline : AnonymizedBox -> Bool
isInline child =
    case child of
        InlineLevel _ ->
            True

        BlockLevel _ ->
            False


wrapInlineBoxInAnonymousBlockForBlockContainer : List AnonymizedBox -> List AnonymizedBox
wrapInlineBoxInAnonymousBlockForBlockContainer children =
    let
        wrapInAnonymousBlock children =
            BlockLevel <| AnonymousBlock children

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


wrapInlineBoxInAnonymousBlockForInlineContainer : Styles -> List AnonymizedBox -> List AnonymizedBox
wrapInlineBoxInAnonymousBlockForInlineContainer styles children =
    wrapInlineBoxInAnonymousBlockForBlockContainer
        ([ InlineLevel <| InlineContainer styles []
         ]
            ++ children
        )
