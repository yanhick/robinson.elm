module LayoutTests exposing (..)

import Dict
import CSSOM
import Style
import Painting
import Layout
import Test exposing (..)
import Expect
import BoxModel


layout : Test
layout =
    describe "layout"
        [ calculateBlockWidth
        , calculateBlockHeight
        , calculateBlockPosition
        , startLayout
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


styles =
    Style.initialStyles


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
                                ({ node = element
                                 , styles =
                                    { styles
                                        | display = CSSOM.Block
                                        , width = CSSOM.WidthLength <| CSSOM.CSSLength 50 CSSOM.Pixel
                                    }
                                 , children = []
                                 }
                                )
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
                                ({ node = element
                                 , styles =
                                    { styles
                                        | display = CSSOM.Block
                                    }
                                 , children = []
                                 }
                                )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthLength <| CSSOM.CSSLength 100 CSSOM.Pixel
                                    , marginLeft = CSSOM.MarginAuto
                                    , marginRight = CSSOM.MarginAuto
                                }
                             , children = []
                             }
                            )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthLength <| CSSOM.CSSLength 100 CSSOM.Pixel
                                    , marginLeft = CSSOM.MarginAuto
                                    , marginRight = CSSOM.MarginAuto
                                }
                             , children = []
                             }
                            )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthLength <| CSSOM.CSSLength 100 CSSOM.Pixel
                                    , marginLeft = CSSOM.MarginLength <| CSSOM.CSSLength 50 CSSOM.Pixel
                                    , marginRight = CSSOM.MarginAuto
                                }
                             , children = []
                             }
                            )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthLength <| CSSOM.CSSLength 100 CSSOM.Pixel
                                    , marginRight = CSSOM.MarginLength <| CSSOM.CSSLength 50 CSSOM.Pixel
                                    , marginLeft = CSSOM.MarginAuto
                                }
                             , children = []
                             }
                            )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthAuto
                                    , marginRight = CSSOM.MarginAuto
                                    , marginLeft = CSSOM.MarginAuto
                                }
                             , children = []
                             }
                            )
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
                            ({ node = element
                             , styles =
                                { styles
                                    | display = CSSOM.Block
                                    , width = CSSOM.WidthAuto
                                    , marginRight = CSSOM.MarginLength <| CSSOM.CSSLength 100 CSSOM.Pixel
                                    , marginLeft = CSSOM.MarginLength <| CSSOM.CSSLength 200 CSSOM.Pixel
                                }
                             , children = []
                             }
                            )
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
                    boxModel =
                        BoxModel.content <|
                            Layout.calculateBlockHeight
                                ({ node = element
                                 , styles =
                                    { styles
                                        | display = CSSOM.Block
                                        , height = CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel
                                    }
                                 , children = []
                                 }
                                )
                                dimensions
                in
                    Expect.equal boxModel.height 50
        , test "do nothing if auto height" <|
            \() ->
                let
                    boxModelContent =
                        BoxModel.content <|
                            Layout.calculateBlockHeight
                                ({ node = element
                                 , styles =
                                    Style.initialStyles
                                 , children = []
                                 }
                                )
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
                                ({ node = element
                                 , styles = Style.initialStyles
                                 , children = []
                                 }
                                )
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
                            [ getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            , getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            ]
                            CSSOM.HeightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    (Layout.LayoutBox { dimensions }) =
                        Layout.startLayout
                            styledNode
                            containingDimensions
                in
                    Expect.true ""
                        ((BoxModel.runBoxModel dimensions).content.height
                            == 100
                        )
        , test "layout itself with padding" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , paddingTop = CSSOM.PaddingLength <| CSSOM.CSSLength 20 CSSOM.Pixel
                                    , paddingBottom = CSSOM.PaddingLength <| CSSOM.CSSLength 20 CSSOM.Pixel
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            , getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            ]
                            CSSOM.HeightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    (Layout.LayoutBox { dimensions }) =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelPadding =
                        BoxModel.paddingBox dimensions
                in
                    Expect.equal
                        boxModelPadding.height
                        220
        , test "layout itself with borders" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , borderTop = Style.Length 20 CSSOM.Pixel
                                    , borderBottom = Style.Length 20 CSSOM.Pixel
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            , getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            ]
                            CSSOM.HeightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    (Layout.LayoutBox { dimensions }) =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelPadding =
                        BoxModel.borderBox dimensions
                in
                    Expect.equal
                        boxModelPadding.height
                        220
        , test "layout itself with margins" <|
            \() ->
                let
                    getStyledNode children height =
                        Style.StyledElement
                            { styles =
                                { styles
                                    | display = CSSOM.Block
                                    , height = height
                                    , marginTop = CSSOM.MarginLength <| CSSOM.CSSLength 20 CSSOM.Pixel
                                    , marginBottom = CSSOM.MarginLength <| CSSOM.CSSLength 20 CSSOM.Pixel
                                }
                            , node = element
                            , children = children
                            }

                    styledNode =
                        getStyledNode
                            [ getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            , getStyledNode [] (CSSOM.HeightLength <| CSSOM.CSSLength 50 CSSOM.Pixel)
                            ]
                            CSSOM.HeightAuto

                    containingDimensions =
                        BoxModel.boxModel
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    (Layout.LayoutBox { dimensions }) =
                        Layout.startLayout
                            styledNode
                            containingDimensions

                    boxModelMargin =
                        BoxModel.marginBox dimensions
                in
                    Expect.equal
                        boxModelMargin.height
                        220
        ]
