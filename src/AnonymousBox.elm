module AnonymousBox exposing (..)

import LayoutBox exposing (..)
import Style exposing (..)
import BoxModel exposing (..)


allBlockChildren : List LayoutBox -> Bool
allBlockChildren =
    List.all
        (\child ->
            case child of
                BlockBox _ ->
                    True

                _ ->
                    False
        )


allInlineChildren : List LayoutBox -> Bool
allInlineChildren =
    List.all
        (\child ->
            case child of
                InlineBox _ ->
                    True

                _ ->
                    False
        )


fixAnonymousChildrenForInlineContainer : Box -> Box -> Box
fixAnonymousChildrenForInlineContainer inlineContainerBox parentBox =
    if allInlineChildren inlineContainerBox.children then
        inlineContainerBox
    else
        { boxModel = parentBox.boxModel
        , styles = parentBox.styles
        , children =
            parentBox.children
                ++ wrapInlineBoxInAnonymousBlockForInlineContainer inlineContainerBox
        }


fixAnonymousChildrenForBlockContainer : List LayoutBox -> List LayoutBox
fixAnonymousChildrenForBlockContainer children =
    if allBlockChildren children || allInlineChildren children then
        children
    else
        wrapInlineBoxInAnonymousBlockForBlockContainer children


isInline : LayoutBox -> Bool
isInline child =
    case child of
        InlineBox _ ->
            True

        _ ->
            False


wrapInlineBoxInAnonymousBlockForBlockContainer : List LayoutBox -> List LayoutBox
wrapInlineBoxInAnonymousBlockForBlockContainer children =
    let
        wrapInAnonymousBlock children =
            AnonymousBox
                { boxModel =
                    initBoxModel
                , styles = initialStyles
                , children = children
                }

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


wrapInlineBoxInAnonymousBlockForInlineContainer : Box -> List LayoutBox
wrapInlineBoxInAnonymousBlockForInlineContainer { styles, boxModel, children } =
    wrapInlineBoxInAnonymousBlockForBlockContainer
        ([ InlineBox
            { boxModel = boxModel
            , styles = styles
            , children = []
            }
         ]
            ++ children
        )
