module AnonymousBoxTests exposing (..)

import Test exposing (..)
import Expect
import LayoutBox
import AnonymousBox
import BoxModel
import Style


blockBox =
    LayoutBox.BlockBox
        { styles = Style.initialStyles
        , boxModel = BoxModel.initBoxModel
        , children = []
        }


inlineBox children =
    LayoutBox.InlineBox
        { styles = Style.initialStyles
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


anonymousBox children =
    LayoutBox.AnonymousBox
        { styles = Style.initialStyles
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


box children =
    { styles = Style.initialStyles
    , boxModel = BoxModel.initBoxModel
    , children = children
    }


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
                        (box [ inlineBox [] ])
                        (box [])
                    )
                    (box [ inlineBox [] ])
        , test "do nothing if no children" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        (box [])
                        (box [])
                    )
                    (box [])
        , test "wrap contiguous inline children in same anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        (box [ inlineBox [], blockBox, inlineBox [], inlineBox [] ])
                        (box [])
                    )
                    (box
                        [ anonymousBox [ inlineBox [], inlineBox [] ]
                        , blockBox
                        , anonymousBox [ inlineBox [], inlineBox [] ]
                        ]
                    )
        , test "attach children to parent container if need to wrap the inline container" <|
            \() ->
                Expect.equal
                    (AnonymousBox.fixAnonymousChildrenForInlineContainer
                        (box [ blockBox ])
                        (box [])
                    )
                    (box [ anonymousBox [ inlineBox [] ], blockBox ])
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
                        (box [ blockBox ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        (box [ blockBox, inlineBox [] ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (AnonymousBox.wrapInlineBoxInAnonymousBlockForInlineContainer
                        (box [ blockBox, inlineBox [] ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        ]
