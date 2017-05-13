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
        AnonymousBox.BlockContainer
            Style.initialStyles
            []


inlineBox children =
    AnonymousBox.InlineLevel <|
        AnonymousBox.InlineContainer
            Style.initialStyles
            children


anonymousBox children =
    AnonymousBox.BlockLevel <|
        AnonymousBox.AnonymousBlock
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


blockLayoutBox children =
    AnonymousBox.BlockLevel <|
        AnonymousBox.BlockContainer
            { styles
                | display = CSSOM.Block
            }
            children


inlineLayoutBox children =
    AnonymousBox.InlineLevel <|
        AnonymousBox.InlineContainer
            { styles
                | display = CSSOM.Inline
            }
            children


anonymousLayoutBox children =
    AnonymousBox.BlockLevel <|
        AnonymousBox.AnonymousBlock
            children


anonymizedTreeOrCrash styledNode =
    case AnonymousBox.boxTree styledNode of
        Nothing ->
            Debug.crash "anonymized tree should not be nothing"

        Just tree ->
            tree


anonymizedTree : Test
anonymizedTree =
    describe "layout tree"
        [ test "wrap inline box in anonymous block for block formatting context" <|
            \() ->
                Expect.equal
                    (anonymizedTreeOrCrash
                        (styledBlockNode [ styledInlineNode [], styledBlockNode [] ])
                    )
                    (blockLayoutBox [ anonymousLayoutBox [ inlineLayoutBox [] ], blockLayoutBox [] ])
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
                    (blockLayoutBox
                        [ anonymousLayoutBox
                            [ inlineLayoutBox [ inlineLayoutBox [] ]
                            ]
                        , blockLayoutBox []
                        , anonymousLayoutBox
                            [ inlineLayoutBox [] ]
                        , blockLayoutBox []
                        , anonymousLayoutBox
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
                    (blockLayoutBox
                        [ anonymousLayoutBox [ inlineLayoutBox [] ]
                        , blockLayoutBox []
                        , anonymousLayoutBox
                            [ inlineLayoutBox []
                            ]
                        ]
                    )
        , test "wrap deep inline box in anonymous block for inline formatting context" <|
            \() ->
                Expect.equal
                    (anonymizedTreeOrCrash
                        (styledBlockNode
                            [ styledInlineNode
                                [ styledBlockNode
                                    [ styledInlineNode []
                                    , styledBlockNode []
                                    ]
                                , styledInlineNode [ styledBlockNode [] ]
                                ]
                            ]
                        )
                    )
                    (blockLayoutBox
                        [ anonymousLayoutBox
                            [ inlineLayoutBox []
                            ]
                        , blockLayoutBox
                            [ anonymousLayoutBox
                                [ inlineLayoutBox []
                                ]
                            , blockLayoutBox []
                            ]
                        , anonymousLayoutBox
                            [ anonymousLayoutBox
                                [ inlineLayoutBox [] ]
                            , blockLayoutBox []
                            ]
                        ]
                    )
        ]
