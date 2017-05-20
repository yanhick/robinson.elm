module BoxTests exposing (..)

import Box
import BoxModel
import CSSOM
import Dict
import Expect
import Style
import Test exposing (..)


styles =
    Style.initialStyles


element =
    { tagName = "div"
    , attributes = Dict.fromList [ ( "foo", "bar" ) ]
    , children = []
    }


blockBox =
    Box.BlockContainerBlockContext
        Style.initialStyles
        []


blockBoxInlineContext children =
    Box.BlockContainerInlineContext
        Style.initialStyles
        []


inlineBox children =
    Box.InlineContainer
        Style.initialStyles
        children


intermediateBlockBox =
    Box.IntermediateBlockContainer
        Style.initialStyles
        []


intermediateInlineBox children =
    Box.IntermediateInlineContainer
        Style.initialStyles
        children


intermediateAnonymousBox children =
    Box.IntermediateAnonymousBlock
        children


boxTests : Test
boxTests =
    describe "wrap inline children in anonymous block"
        [ test "wrap inline children in anonymous block for block container with inline element at the end" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateInlineBox [], intermediateBlockBox, intermediateInlineBox [] ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    , intermediateAnonymousBox [ intermediateInlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for block container" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateInlineBox [], intermediateBlockBox ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    ]
        , test "wrap contiguous inline children in same anonymous block" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForBlockContainer
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
                    (Box.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateBlockBox ]
                    )
                    [ intermediateBlockBox ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ intermediateBlockBox ]
                    )
                    [ intermediateBlockBox ]
        , test "do nothing if all children inline for inline container" <|
            \() ->
                Expect.equal
                    (Box.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ intermediateInlineBox [] ]
                    )
                    Nothing
        , test "do nothing if no children" <|
            \() ->
                Expect.equal
                    (Box.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        []
                    )
                    Nothing
        , test "wrap contiguous inline children in same anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (Box.fixAnonymousChildrenForInlineContainer
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
                    (Box.fixAnonymousChildrenForInlineContainer
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
                    (Box.fixAnonymousChildrenForBlockContainer
                        [ intermediateBlockBox, intermediateBlockBox ]
                    )
                    [ intermediateBlockBox, intermediateBlockBox ]
        , test "do nothing if all children inline for block container" <|
            \() ->
                Expect.equal
                    (Box.fixAnonymousChildrenForBlockContainer
                        [ intermediateInlineBox [], intermediateInlineBox [] ]
                    )
                    [ intermediateInlineBox [], intermediateInlineBox [] ]
        , test "wrap inline children in anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ intermediateBlockBox ]
                    )
                    [ intermediateAnonymousBox [ intermediateInlineBox [] ]
                    , intermediateBlockBox
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (Box.wrapInlineBoxInAnonymousBlockForInlineContainer
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
                    (Box.wrapInlineBoxInAnonymousBlockForInlineContainer
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
    Box.BoxRoot
        { styles
            | display = CSSOM.Block
        }
        [ child ]


blockLayoutBox children =
    Box.BlockContainerBlockContext
        { styles
            | display = CSSOM.Block
        }
        children


blockLayoutBoxInlineContext children =
    Box.BlockContainerInlineContext
        { styles
            | display = CSSOM.Block
        }
        children


inlineLevelLayoutBox children =
    Box.InlineContainer
        { styles
            | display = CSSOM.Inline
        }
        children


inlineLayoutBox children =
    Box.InlineContainer
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


dumpBoxTree (Box.BoxRoot styles children) =
    DumpRoot
        (List.map
            dumpBoxTreeChildren
            children
        )


dumpBoxTreeChildren child =
    case child of
        Box.BlockContainerBlockContext styles children ->
            DumpBlockContainerBlockContext
                (List.map dumpBoxTreeChildren children)

        Box.BlockContainerInlineContext styles children ->
            DumpBlockContainerInlineContext
                (List.map dumpBoxTreeInlineChildren children)


dumpBoxTreeInlineChildren child =
    case child of
        Box.InlineContainer styles children ->
            DumpInlineContainer (List.map dumpBoxTreeInlineChildren children)

        Box.InlineText text ->
            DumpText


anonymizedTreeOrCrash styledNode =
    Box.boxTree
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
                    (dumpBoxTree <|
                        anonymizedTreeOrCrash
                            (styledBlockNode [ styledInlineNode [], styledBlockNode [] ])
                    )
                    (dumpBoxTree <| blockRoot <| blockLayoutBox [ blockLayoutBoxInlineContext [ inlineLayoutBox [] ], blockLayoutBox [] ])
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
                            [ blockLayoutBoxInlineContext
                                [ inlineLayoutBox [ inlineLayoutBox [] ]
                                ]
                            , blockLayoutBox []
                            , blockLayoutBoxInlineContext
                                [ inlineLayoutBox [] ]
                            , blockLayoutBox []
                            , blockLayoutBoxInlineContext
                                [ inlineLayoutBox [] ]
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
                            [ blockLayoutBoxInlineContext [ inlineLayoutBox [] ]
                            , blockLayoutBox []
                            , blockLayoutBoxInlineContext
                                [ inlineLayoutBox []
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
                                [ blockLayoutBoxInlineContext
                                    [ inlineLayoutBox []
                                    ]
                                , blockLayoutBox
                                    [ blockLayoutBoxInlineContext
                                        [ inlineLayoutBox [ inlineLayoutBox [] ]
                                        ]
                                    , blockLayoutBox []
                                    ]
                                , blockLayoutBox
                                    [ blockLayoutBoxInlineContext
                                        [ inlineLayoutBox [] ]
                                    , blockLayoutBox []
                                    ]
                                ]
                    )
        ]
