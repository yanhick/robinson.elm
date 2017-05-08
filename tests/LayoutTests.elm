module LayoutTests exposing (..)

import Dict
import CSSOM
import Style
import Painting
import Layout
import Test exposing (..)
import Expect
import BoxModel
import CSSBasicTypes


layout : Test
layout =
    describe "layout"
        [ calculateBlockWidth
        , calculateBlockHeight
        , calculateBlockPosition
        , layoutBlockChildren
        , startLayout
        , fixAnonymousChildrenForBlockContainer
        ]


edgeSize =
    { top = 0, right = 0, bottom = 0, left = 0 }


element =
    { tagName = "div"
    , attributes = Dict.fromList [ ( "foo", "bar" ) ]
    , children = []
    }


dimensions =
    BoxModel.boxModel
        { x = 0, y = 0, width = 0, height = 0 }
        edgeSize
        edgeSize
        edgeSize


testCSSLength length =
    case CSSBasicTypes.cssPixelLength length of
        Just validLength ->
            validLength

        Nothing ->
            CSSBasicTypes.defaultCSSLength


styles =
    Style.initialStyles


getBoxModel layoutBox =
    case layoutBox of
        Layout.BlockBox { boxModel } ->
            Just boxModel

        Layout.InlineBox { boxModel } ->
            Just boxModel

        Layout.AnonymousBox { boxModel } ->
            Just boxModel

        Layout.TextBox _ ->
            Nothing


exampleLength =
    Maybe.withDefault CSSBasicTypes.defaultCSSLength (CSSBasicTypes.cssPixelLength 100)


calculateBlockWidth : Test
calculateBlockWidth =
    describe "calculate block width"
        [ test "set the block width with an explicit width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockWidth
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.widthLength <| testCSSLength 50
                                }
                                dimensions
                                containingDimensions
                in
                    Expect.equal boxModelContent.width 50
        , test "set the block width with an auto width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockWidth
                                { styles
                                    | display = CSSOM.Block
                                }
                                dimensions
                                containingDimensions
                in
                    Expect.equal boxModelContent.width 200
        , test "set the margins width with auto margins and explicit width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginAuto
                                , marginRight = CSSOM.marginAuto
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 100
                            && boxModelMargin.left
                            == 50
                            && boxModelMargin.right
                            == 50
                        )
        , test "set the margins width with auto margins and explicit width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginAuto
                                , marginRight = CSSOM.marginAuto
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 100
                            && boxModelMargin.left
                            == 50
                            && boxModelMargin.right
                            == 50
                        )
        , test "resize left auto margin when right margin and width length is explicit" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginLength <| testCSSLength 50
                                , marginRight = CSSOM.marginAuto
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 100
                            && boxModelMargin.left
                            == 50
                            && boxModelMargin.right
                            == -50
                        )
        , test "resize right auto margin when left margin and width length is explicit" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginRight = CSSOM.marginLength <| testCSSLength 50
                                , marginLeft = CSSOM.marginAuto
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 100
                            && boxModelMargin.left
                            == 50
                            && boxModelMargin.right
                            == 50
                        )
        , test "set auto margins to 0 if width is auto" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthAuto
                                , marginRight = CSSOM.marginAuto
                                , marginLeft = CSSOM.marginAuto
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 200
                            && boxModelMargin.left
                            == 0
                            && boxModelMargin.right
                            == 0
                        )
        , test "make right margin negative if the width was going to be negative" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModel =
                        Layout.calculateBlockWidth
                            { styles
                                | display = CSSOM.Block
                                , width = CSSOM.widthAuto
                                , marginRight = CSSOM.marginLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginLength <| testCSSLength 200
                            }
                            dimensions
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel

                    boxModelMargin =
                        BoxModel.margin boxModel
                in
                    Expect.true ""
                        (boxModelContent.width
                            == 0
                            && boxModelMargin.left
                            == 200
                            && boxModelMargin.right
                            == -100
                        )
        ]


calculateBlockHeight : Test
calculateBlockHeight =
    describe "calculate block height"
        [ test "set the block height if provided" <|
            \() ->
                let
                    newStyles =
                        { styles
                            | display = CSSOM.Block
                            , height = CSSOM.heightLength <| testCSSLength 50
                        }

                    boxModel =
                        BoxModel.content <|
                            Layout.calculateBlockHeight
                                newStyles
                                dimensions
                in
                    Expect.equal boxModel.height 50
        , test "do nothing if auto height" <|
            \() ->
                let
                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockHeight
                                Style.initialStyles
                                dimensions
                in
                    Expect.equal boxModelContent.height 0
        ]


calculateBlockPosition : Test
calculateBlockPosition =
    describe "calculate block position"
        [ test "set the block position" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 10, y = 20, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockPosition
                                Style.initialStyles
                                dimensions
                                containingDimensions
                in
                    Expect.true ""
                        (boxModelContent.x
                            == 10
                            && boxModelContent.y
                            == 20
                        )
        ]


layoutBlockChildren : Test
layoutBlockChildren =
    describe "layout block children"
        [ test "compute the containing block height" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    getBlockBox children height =
                        Layout.BlockBox
                            { boxModel = dimensions
                            , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                }
                            , children = children
                            }

                    ( laidoutChildren, childrenBoxModel ) =
                        Layout.layoutBlockChildren
                            [ (getBlockBox [] <| CSSOM.heightLength exampleLength)
                            , (getBlockBox [] <| CSSOM.heightLength exampleLength)
                            ]
                            dimensions
                            containingDimensions
                in
                    Expect.equal
                        (BoxModel.content childrenBoxModel).height
                        200
        ]


startLayout : Test
startLayout =
    describe "start layout"
        [ test "layout itself" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    layoutBox =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelContent =
                        Maybe.map BoxModel.content <| getBoxModel layoutBox
                in
                    Expect.equal
                        (Maybe.map .height boxModelContent)
                        (Just 100)
        , test "layout itself with padding" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , paddingTop = CSSOM.padding <| testCSSLength 20
                                    , paddingBottom = CSSOM.padding <| testCSSLength 20
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    layoutBox =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelPadding =
                        Maybe.map BoxModel.paddingBox <| getBoxModel layoutBox
                in
                    Expect.equal
                        (Maybe.map .height boxModelPadding)
                        (Just 220)
        , test "layout itself with borders" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , borderTopWidth = CSSOM.borderWidthLength <| testCSSLength 20
                                    , borderBottomWidth = CSSOM.borderWidthLength <| testCSSLength 20
                                    , borderTopStyle = CSSOM.borderStyleSolid
                                    , borderBottomStyle = CSSOM.borderStyleSolid
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    layoutBox =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelBorder =
                        Maybe.map BoxModel.borderBox <| getBoxModel layoutBox
                in
                    Expect.equal
                        (Maybe.map .height boxModelBorder)
                        (Just 220)
        , test "layout itself with margins" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , marginTop = CSSOM.marginLength <| testCSSLength 20
                                    , marginBottom = CSSOM.marginLength <| testCSSLength 20
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    layoutBox =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelMargin =
                        Maybe.map BoxModel.marginBox <| getBoxModel layoutBox
                in
                    Expect.equal
                        (Maybe.map .height boxModelMargin)
                        (Just 220)
        ]


blockBox =
    Layout.BlockBox
        { styles = Style.initialStyles
        , boxModel = dimensions
        , children = []
        }


inlineBox children =
    Layout.InlineBox
        { styles = Style.initialStyles
        , boxModel = dimensions
        , children = children
        }


anonymousBox children =
    Layout.AnonymousBox
        { styles = Style.initialStyles
        , boxModel = dimensions
        , children = children
        }


box children =
    { styles = Style.initialStyles
    , boxModel = dimensions
    , children = children
    }


fixAnonymousChildrenForBlockContainer =
    describe "wrap inline children in anonymous block"
        [ test "wrap inline children in anonymous block for block container with inline element at the end" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ inlineBox [], blockBox, inlineBox [] ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for block container" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ inlineBox [], blockBox ]
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    ]
        , test "wrap contiguous inline children in same anonymous block" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ blockBox, inlineBox [], inlineBox [], blockBox ]
                    )
                    [ blockBox
                    , anonymousBox [ inlineBox [], inlineBox [] ]
                    , blockBox
                    ]
        , test "do nothing if single block" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForBlockContainer
                        [ blockBox ]
                    )
                    [ blockBox ]
        , test "do nothing if all children inline for inline container" <|
            \() ->
                Expect.equal
                    (Layout.fixAnonymousChildrenForInlineContainer
                        (box [ inlineBox [] ])
                        (box [])
                    )
                    (box [ inlineBox [] ])
        , test "wrap contiguous inline children in same anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (Layout.fixAnonymousChildrenForInlineContainer
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
                    (Layout.fixAnonymousChildrenForInlineContainer
                        (box [ blockBox ])
                        (box [])
                    )
                    (box [ anonymousBox [ inlineBox [] ], blockBox ])
        , test "do nothing if all children block for block container" <|
            \() ->
                Expect.equal
                    (Layout.fixAnonymousChildrenForBlockContainer
                        [ blockBox, blockBox ]
                    )
                    [ blockBox, blockBox ]
        , test "do nothing if all children inline for block container" <|
            \() ->
                Expect.equal
                    (Layout.fixAnonymousChildrenForBlockContainer
                        [ inlineBox [], inlineBox [] ]
                    )
                    [ inlineBox [], inlineBox [] ]
        , test "wrap inline children in anonymous block for inline container" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForInlineContainer
                        (box [ blockBox ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForInlineContainer
                        (box [ blockBox, inlineBox [] ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        , test "wrap inline children in anonymous block for inline container with inline element last" <|
            \() ->
                Expect.equal
                    (Layout.wrapInlineBoxInAnonymousBlockForInlineContainer
                        (box [ blockBox, inlineBox [] ])
                    )
                    [ anonymousBox [ inlineBox [] ]
                    , blockBox
                    , anonymousBox [ inlineBox [] ]
                    ]
        ]
