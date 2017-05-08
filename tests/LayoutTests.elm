module LayoutTests exposing (..)

import Dict
import CSSOM
import Style
import Painting
import AnonymousBox
import Layout
import LayoutBox
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
        , layoutTree
        ]


edgeSize =
    { top = 0, right = 0, bottom = 0, left = 0 }


element =
    { tagName = "div"
    , attributes = Dict.fromList [ ( "foo", "bar" ) ]
    , children = []
    }


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
        LayoutBox.BlockBox { boxModel } ->
            Just boxModel

        LayoutBox.InlineBox { boxModel } ->
            Just boxModel

        LayoutBox.AnonymousBox { boxModel } ->
            Just boxModel

        LayoutBox.AnonymousBoxInlineRoot { boxModel } ->
            Just boxModel

        LayoutBox.TextBox _ ->
            Nothing


exampleLength =
    Maybe.withDefault CSSBasicTypes.defaultCSSLength (CSSBasicTypes.cssPixelLength 100)


type BoxType
    = Block (List BoxType)
    | Inline (List BoxType)
    | Anonymous (List BoxType)
    | AnonymousInlineRoot (List BoxType)
    | Text


getBoxType layoutBox =
    case layoutBox of
        LayoutBox.BlockBox { children } ->
            Block (List.map getBoxType children)

        LayoutBox.InlineBox { children } ->
            Inline (List.map getBoxType children)

        LayoutBox.AnonymousBox { children } ->
            Anonymous (List.map getBoxType children)

        LayoutBox.AnonymousBoxInlineRoot { children } ->
            AnonymousInlineRoot (List.map getBoxType children)

        LayoutBox.TextBox _ ->
            Text


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
                                BoxModel.initBoxModel
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
                                BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                                BoxModel.initBoxModel
                in
                    Expect.equal boxModel.height 50
        , test "do nothing if auto height" <|
            \() ->
                let
                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockHeight
                                Style.initialStyles
                                BoxModel.initBoxModel
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
                                BoxModel.initBoxModel
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
                        LayoutBox.BlockBox
                            { boxModel = BoxModel.initBoxModel
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
                            BoxModel.initBoxModel
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
                        case
                            Layout.startLayout
                                styledNode
                                containingDimensions
                        of
                            Err _ ->
                                Debug.crash "layout box should not be nothing"

                            Ok box ->
                                box

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
                        case
                            Layout.startLayout
                                styledNode
                                containingDimensions
                        of
                            Err _ ->
                                Debug.crash "layout box should not be nothing"

                            Ok box ->
                                box

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
                        case
                            Layout.startLayout
                                styledNode
                                containingDimensions
                        of
                            Err _ ->
                                Debug.crash "layout box should not be nothing"

                            Ok box ->
                                box

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
                        case
                            Layout.startLayout
                                styledNode
                                containingDimensions
                        of
                            Err _ ->
                                Debug.crash "layout box should not be nothing"

                            Ok box ->
                                box

                    boxModelMargin =
                        Maybe.map BoxModel.marginBox <| getBoxModel layoutBox
                in
                    Expect.equal
                        (Maybe.map .height boxModelMargin)
                        (Just 220)
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
    LayoutBox.BlockBox
        { styles =
            { styles
                | display = CSSOM.Block
            }
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


inlineLayoutBox children =
    LayoutBox.InlineBox
        { styles =
            { styles
                | display = CSSOM.Inline
            }
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


anonymousLayoutBox children =
    LayoutBox.AnonymousBox
        { styles =
            { styles
                | display = CSSOM.Block
            }
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


anonymousInlineRootLayoutBox children =
    LayoutBox.AnonymousBoxInlineRoot
        { styles =
            { styles
                | display = CSSOM.Block
            }
        , boxModel = BoxModel.initBoxModel
        , children = children
        }


layoutTreeOrCrash styledNode =
    case Layout.layoutTree styledNode of
        Nothing ->
            Debug.crash "layout tree should not be nothing"

        Just tree ->
            tree


layoutTree : Test
layoutTree =
    describe "layout tree"
        [ test "wrap inline box in anonymous block for block formatting context" <|
            \() ->
                Expect.equal
                    (getBoxType <|
                        layoutTreeOrCrash
                            (styledBlockNode [ styledInlineNode [], styledBlockNode [] ])
                    )
                    (getBoxType <|
                        blockLayoutBox [ anonymousLayoutBox [ inlineLayoutBox [] ], blockLayoutBox [] ]
                    )
        , test "wrap deep inline box in anonymous block for block formatting context" <|
            \() ->
                Expect.equal
                    (getBoxType <|
                        layoutTreeOrCrash
                            (styledBlockNode
                                [ styledInlineNode [ styledInlineNode [] ]
                                , styledBlockNode []
                                , styledInlineNode []
                                , styledBlockNode []
                                , styledInlineNode []
                                ]
                            )
                    )
                    (getBoxType <|
                        blockLayoutBox
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
                    (getBoxType <|
                        layoutTreeOrCrash
                            (styledInlineNode
                                [ styledBlockNode []
                                , styledInlineNode []
                                ]
                            )
                    )
                    (getBoxType <|
                        anonymousInlineRootLayoutBox
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
                    (getBoxType <|
                        layoutTreeOrCrash
                            (styledInlineNode
                                [ styledBlockNode
                                    [ styledInlineNode []
                                    , styledBlockNode []
                                    ]
                                , styledInlineNode [ styledBlockNode [] ]
                                ]
                            )
                    )
                    (getBoxType <|
                        anonymousInlineRootLayoutBox
                            [ anonymousLayoutBox
                                [ inlineLayoutBox []
                                ]
                            , blockLayoutBox
                                [ anonymousLayoutBox
                                    [ inlineLayoutBox []
                                    ]
                                , blockLayoutBox []
                                ]
                            , anonymousInlineRootLayoutBox
                                [ anonymousLayoutBox
                                    [ inlineLayoutBox [] ]
                                , blockLayoutBox []
                                ]
                            ]
                    )
        ]
