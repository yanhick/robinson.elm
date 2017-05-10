module AnonymousBoxTests exposing (..)

import Test exposing (..)
import Expect
import LayoutBox
import AnonymousBox
import BoxModel
import Style


blockBox =
    LayoutBox.BlockLevel <|
        LayoutBox.BlockContainer
            Style.initialStyles
            []


inlineBox children =
    LayoutBox.InlineLevel <|
        LayoutBox.InlineContainer
            Style.initialStyles
            children


anonymousBox children =
    LayoutBox.BlockLevel <|
        LayoutBox.AnonymousBlock
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
        ]
