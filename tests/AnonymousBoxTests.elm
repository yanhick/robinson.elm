module AnonymousBoxTests exposing (..)

import Test exposing (..)
import Expect
import Dict
import AnonymousBox
import BoxModel
import Style
import CSSOM


styles =
    Style.initialStyles


element =
    { tagName = "div"
    , attributes = Dict.fromList [ ( "foo", "bar" ) ]
    , children = []
    }


blockBox =
    AnonymousBox.BlockLevel <|
        AnonymousBox.BlockContainerBlockContext
            Style.initialStyles
            []


inlineBox children =
    AnonymousBox.InlineLevel <|
        AnonymousBox.InlineContainer
            Style.initialStyles
            children


intermediateBlockBox =
    AnonymousBox.IntermediateBlockContainer
        Style.initialStyles
        []


intermediateInlineBox children =
    AnonymousBox.IntermediateInlineContainer
        Style.initialStyles
        children


intermediateAnonymousBox children =
    AnonymousBox.IntermediateAnonymousBlock
        children


anonymousBoxTests : Test
anonymousBoxTests =
    describe "wrap inline children in anonymous block"
        [ test "wrap inline children in anonymous block for block container with inline element at the end" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateInlineBox [], intermediateBlockBox, intermediateInlineBox [] ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    , intermediateAnonymousBox [ intermediateInlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateInlineBox [], intermediateBlockBox ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    ]
        , test "wrap contiguous inline children in same anonymous block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateBlockBox
                        , intermediateInlineBox []
                        , intermediateInlineBox []
                        , intermediateBlockBox
                        ]
                    )
                    [ intermediateBlockBox
                    , intermediateAnonymousBox
                        [ intermediateInlineBox []
                        , intermediateInlineBox []
                        ]
                    , intermediateBlockBox
                    ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateBlockBox ]
                    )
                    [ intermediateBlockBox ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateBlockBox ]
                    )
                    [ intermediateBlockBox ]
        , test "do nothing if all children inline for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ intermediateInlineBox [] ]
                    )
                    Nothing
        , test "do nothing if no children" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        []
                    )
                    Nothing
        , test "wrap contiguous inline children in same anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ intermediateInlineBox []
                        , intermediateBlockBox
                        , intermediateInlineBox []
                        , intermediateInlineBox []
                        ]
                    )
                    (Just
                        [ intermediateAnonymousBox
                            [ intermediateInlineBox []
                            , intermediateInlineBox []
                            ]
                        , intermediateBlockBox
                        , intermediateAnonymousBox
                            [ intermediateInlineBox []
                            , intermediateInlineBox []
                            ]
                        ]
                    )
        , test "attach children to parent container if need to wrap the inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ intermediateBlockBox ]
                    )
                    (Just
                        [ intermediateAnonymousBox
                            [ intermediateInlineBox []
                            ]
                        , intermediateBlockBox
                        ]
                    )
        , test "do nothing if all children block for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForBlockContainer
                        [ intermediateBlockBox, intermediateBlockBox ]
                    )
                    [ intermediateBlockBox, intermediateBlockBox ]
        , test "do nothing if all children inline for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForBlockContainer
                        [ intermediateInlineBox [], intermediateInlineBox [] ]
                    )
                    [ intermediateInlineBox [], intermediateInlineBox [] ]
        , test "wrap inline children in anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ intermediateBlockBox ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ intermediateBlockBox, intermediateInlineBox [] ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    , intermediateAnonymousBox [ intermediateInlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ intermediateBlockBox, intermediateInlineBox [] ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    , intermediateAnonymousBox [ intermediateInlineBox [] ]
                    ]
        , anonymizedTree
        ]


styledBlockNode children =
    Style.StyledElement
        { styles =
            { styles
                | display = CSSOM.Block
            }
        , node = element
        , children = children
        }


styledInlineNode children =
    Style.StyledElement
        { styles =
            { styles
                | display = CSSOM.Inline
            }
        , node = element
        , children = children
        }


styledRootNode children =
    Style.StyledRoot
        { styles =
            { styles
                | display = CSSOM.Inline
            }
        , node = element
        , children = children
        }


blockRoot child =
    AnonymousBox.BoxRoot
        { styles
            | display = CSSOM.Block
        }
        [ child ]


blockLayoutBox children =
    AnonymousBox.BlockLevel <|
        AnonymousBox.BlockContainerBlockContext
            { styles
                | display = CSSOM.Block
            }
            children


inlineLevelLayoutBox children =
    AnonymousBox.InlineLevel <|
        AnonymousBox.InlineContainer
            { styles
                | display = CSSOM.Inline
            }
            children


inlineLayoutBox children =
    AnonymousBox.InlineContainer
        { styles
            | display = CSSOM.Inline
        }
        children


type DumpBoxTree
    = DumpRoot (List DumpBoxTree)
    | DumpBlockContainerBlockContext (List DumpBoxTree)
    | DumpBlockContainerInlineContext (List DumpBoxTree)
    | DumpInlineContainer (List DumpBoxTree)
    | DumpText


dumpBoxTree (AnonymousBox.BoxRoot styles children) =
    DumpRoot
        (List.map
            dumpBoxTreeChildren
            children
        )


dumpBoxTreeChildren child =
    case child of
        AnonymousBox.BlockLevel blockLevel ->
            case blockLevel of
                AnonymousBox.BlockContainerBlockContext styles children ->
                    DumpBlockContainerBlockContext
                        (List.map dumpBoxTreeChildren children)

                AnonymousBox.BlockContainerInlineContext styles children ->
                    DumpBlockContainerInlineContext
                        (List.map dumpBoxTreeInlineChildren children)

        AnonymousBox.InlineLevel inlineLevel ->
            case inlineLevel of
                AnonymousBox.InlineText text ->
                    DumpText

                AnonymousBox.InlineContainer styles children ->
                    DumpInlineContainer (List.map dumpBoxTreeInlineChildren children)


dumpBoxTreeInlineChildren child =
    case child of
        AnonymousBox.InlineContainer styles children ->
            DumpInlineContainer (List.map dumpBoxTreeInlineChildren children)

        AnonymousBox.InlineText text ->
            DumpText


anonymizedTreeOrCrash styledNode =
    AnonymousBox.boxTree
        (Style.StyledRoot
            { styles =
                { styles
                    | display = CSSOM.Block
                }
            , node = element
            , children = [ styledNode ]
            }
        )


anonymizedTree : Test
anonymizedTree =
    describe "layout tree"
        [ test "wrap inline box in anonymous block for block formatting context" <|
            \() ->
                Expect.equal
                    (anonymizedTreeOrCrash
                        (styledBlockNode [ styledInlineNode [], styledBlockNode [] ])
                    )
                    (blockRoot <| blockLayoutBox [ blockLayoutBox [ inlineLevelLayoutBox [] ], blockLayoutBox [] ])
        , test "wrap deep inline box in anonymous block for block formatting context" <|
            \() ->
                Expect.equal
                    (anonymizedTreeOrCrash
                        (styledBlockNode
                            [ styledInlineNode [ styledInlineNode [] ]
                            , styledBlockNode []
                            , styledInlineNode []
                            , styledBlockNode []
                            , styledInlineNode []
                            ]
                        )
                    )
                    (blockRoot <|
                        blockLayoutBox
                            [ blockLayoutBox
                                [ inlineLevelLayoutBox [ inlineLayoutBox [] ]
                                ]
                            , blockLayoutBox []
                            , blockLayoutBox
                                [ inlineLevelLayoutBox [] ]
                            , blockLayoutBox []
                            , blockLayoutBox
                                [ inlineLevelLayoutBox [] ]
                            ]
                    )
        , test "wrap inline box in anonymous block for inline formatting context" <|
            \() ->
                Expect.equal
                    (anonymizedTreeOrCrash
                        (styledBlockNode
                            [ styledInlineNode
                                [ styledBlockNode []
                                , styledInlineNode []
                                ]
                            ]
                        )
                    )
                    (blockRoot <|
                        blockLayoutBox
                            [ blockLayoutBox [ inlineLevelLayoutBox [] ]
                            , blockLayoutBox []
                            , blockLayoutBox
                                [ inlineLevelLayoutBox []
                                ]
                            ]
                    )
        , test "wrap deep inline box in anonymous block for inline formatting context" <|
            \() ->
                Expect.equal
                    (dumpBoxTree <|
                        anonymizedTreeOrCrash
                            (styledBlockNode
                                [ styledInlineNode
                                    [ styledBlockNode
                                        [ styledInlineNode [ styledInlineNode [] ]
                                        , styledBlockNode []
                                        ]
                                    , styledInlineNode [ styledBlockNode [] ]
                                    ]
                                ]
                            )
                    )
                    (dumpBoxTree <|
                        blockRoot <|
                            blockLayoutBox
                                [ blockLayoutBox
                                    [ inlineLevelLayoutBox []
                                    ]
                                , blockLayoutBox
                                    [ blockLayoutBox
                                        [ inlineLevelLayoutBox [ inlineLayoutBox [] ]
                                        ]
                                    , blockLayoutBox []
                                    ]
                                , blockLayoutBox
                                    [ blockLayoutBox
                                        [ inlineLevelLayoutBox [] ]
                                    , blockLayoutBox []
                                    ]
                                ]
                    )
        ]
