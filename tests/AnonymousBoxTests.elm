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


anonymousBoxTests : Test
anonymousBoxTests =
    describe "wrap inline children in anonymous block"
        [ test "wrap inline children in anonymous block for block container with inline element at the end" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ inlineBox [], blockBox, inlineBox [] ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ inlineBox [], blockBox ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    ]
        , test "wrap contiguous inline children in same anonymous block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ blockBox, inlineBox [], inlineBox [], blockBox ]
                    )
                    [ blockBox
                    , anonymousBox [ inlineBox [], inlineBox [] ]
                    , blockBox
                    ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ blockBox ]
                    )
                    [ blockBox ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ blockBox ]
                    )
                    [ blockBox ]
        , test "do nothing if all children inline for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ inlineBox [] ]
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
                        [ inlineBox [], blockBox, inlineBox [], inlineBox [] ]
                    )
                    (Just
                        [ anonymousBox [ inlineBox [], inlineBox [] ]
                        , blockBox
                        , anonymousBox [ inlineBox [], inlineBox [] ]
                        ]
                    )
        , test "attach children to parent container if need to wrap the inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        Style.initialStyles
                        [ blockBox ]
                    )
                    (Just [ anonymousBox [ inlineBox [] ], blockBox ])
        , test "do nothing if all children block for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForBlockContainer
                        [ blockBox, blockBox ]
                    )
                    [ blockBox, blockBox ]
        , test "do nothing if all children inline for block container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForBlockContainer
                        [ inlineBox [], inlineBox [] ]
                    )
                    [ inlineBox [], inlineBox [] ]
        , test "wrap inline children in anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ blockBox ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ blockBox, inlineBox [] ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        Style.initialStyles
                        [ blockBox, inlineBox [] ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
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


anonymousInlineRootLayoutBox children =
    AnonymousBox.InlineLevel <|
        AnonymousBox.AnonymousInlineRoot
            children


anonymizedTreeOrCrash styledNode =
    case AnonymousBox.anonymizedTree styledNode of
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
                        (styledInlineNode
                            [ styledBlockNode []
                            , styledInlineNode []
                            ]
                        )
                    )
                    (anonymousInlineRootLayoutBox
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
                        (styledInlineNode
                            [ styledBlockNode
                                [ styledInlineNode []
                                , styledBlockNode []
                                ]
                            , styledInlineNode [ styledBlockNode [] ]
                            ]
                        )
                    )
                    (anonymousInlineRootLayoutBox
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
                            [ anonymousInlineRootLayoutBox
                                [ anonymousLayoutBox
                                    [ inlineLayoutBox [] ]
                                , blockLayoutBox []
                                ]
                            ]
                        ]
                    )
        ]
