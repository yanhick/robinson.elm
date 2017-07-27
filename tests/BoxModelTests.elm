module BoxModelTests exposing (..)

import BoxModel exposing (..)
import CSSBasicTypes
import CSSOM
import Expect
import Fuzz exposing (..)
import Style
import Test exposing (..)


box : BoxModel
box =
    make
        { x = 0, y = 0, width = 100, height = 100 }
        { top = 10, right = 10, bottom = 10, left = 10 }
        { top = 10, right = 10, bottom = 10, left = 10 }
        { top = 10, right = 10, bottom = 10, left = 10 }


fuzzRect : Fuzzer Rect
fuzzRect =
    Fuzz.map4
        Rect
        (floatRange 0 1000)
        (floatRange 0 1000)
        (floatRange 0 1000)
        (floatRange 0 1000)


fuzzEdgeSize : Fuzzer EdgeSize
fuzzEdgeSize =
    Fuzz.map4
        EdgeSize
        (floatRange 0 1000)
        (floatRange 0 1000)
        (floatRange 0 1000)
        (floatRange 0 1000)


fuzzBoxModel : Fuzzer BoxModel
fuzzBoxModel =
    Fuzz.map4
        make
        fuzzRect
        fuzzEdgeSize
        fuzzEdgeSize
        fuzzEdgeSize


styles =
    Style.initialStyles


testCSSLength length =
    case CSSBasicTypes.cssPixelLength length of
        Just validLength ->
            validLength

        Nothing ->
            CSSBasicTypes.defaultCSSLength


boxModelTests : Test
boxModelTests =
    describe "Box Model"
        [ test "get padding box" <|
            \() ->
                Expect.equal
                    (paddingBox box)
                    { x = -10, y = -10, width = 120, height = 120 }
        , test "get border box" <|
            \() ->
                Expect.equal
                    (borderBox box)
                    { x = -20, y = -20, width = 140, height = 140 }
        , test "get margin box" <|
            \() ->
                Expect.equal
                    (marginBox box)
                    { x = -30, y = -30, width = 160, height = 160 }
        , fuzz fuzzBoxModel "padding box should create larger rect" <|
            \boxModel ->
                let
                    { width, height } =
                        content boxModel

                    paddingBoxRect =
                        paddingBox boxModel
                in
                Expect.true "area of padding box is larger or equal to area of content box"
                    ((width * height) <= (paddingBoxRect.width * paddingBoxRect.height))
        ]


blockPositionTests : Test
blockPositionTests =
    describe "calculate block position inside it's container"
        [ test "get the block position" <|
            \() ->
                let
                    containingDimensions =
                        make
                            { x = 10, y = 20, width = 0, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModelContent =
                        content <|
                            blockPosition
                                Style.initialStyles
                                default
                                containingDimensions
                in
                Expect.true "block position is correct"
                    (boxModelContent.x
                        == 10
                        && boxModelContent.y
                        == 20
                    )
        ]


blockHeightTests : Test
blockHeightTests =
    describe "calculate block height"
        [ test "set the block height if provided" <|
            \() ->
                let
                    newStyles =
                        { styles
                            | display = CSSOM.displayBlock
                            , height = CSSOM.heightLength <| testCSSLength 50
                        }

                    boxModel =
                        content <|
                            blockHeight
                                newStyles
                                default
                in
                Expect.equal boxModel.height 50
        , test "do nothing if auto height" <|
            \() ->
                let
                    boxModelContent =
                        content <|
                            blockHeight
                                Style.initialStyles
                                default
                in
                Expect.equal boxModelContent.height 0
        ]


blockWidthTests : Test
blockWidthTests =
    describe "calculate block width"
        [ test "set the block width with an explicit width" <|
            \() ->
                let
                    containingDimensions =
                        make
                            { x = 0, y = 0, width = 100, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModelContent =
                        content <|
                            blockWidth
                                { styles
                                    | display = CSSOM.displayBlock
                                    , width = CSSOM.widthLength <| testCSSLength 50
                                }
                                BoxModel.default
                                containingDimensions
                in
                Expect.equal boxModelContent.width 50
        , test "set the block width with an auto width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 200, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModelContent =
                        BoxModel.content <|
                            blockWidth
                                { styles
                                    | display = CSSOM.displayBlock
                                }
                                BoxModel.default
                                containingDimensions
                in
                Expect.equal boxModelContent.width 200
        , test "set the margins width with auto margins and explicit width" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 200, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModel =
                        blockWidth
                            { styles
                                | display = CSSOM.displayBlock
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginAuto
                                , marginRight = CSSOM.marginAuto
                            }
                            BoxModel.default
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
                        BoxModel.make
                            { x = 0, y = 0, width = 100, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModel =
                        blockWidth
                            { styles
                                | display = CSSOM.displayBlock
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginLength <| testCSSLength 50
                                , marginRight = CSSOM.marginAuto
                            }
                            default
                            containingDimensions

                    boxModelContent =
                        content boxModel

                    boxModelMargin =
                        margin boxModel
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
                        make
                            { x = 0, y = 0, width = 200, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModel =
                        blockWidth
                            { styles
                                | display = CSSOM.displayBlock
                                , width = CSSOM.widthLength <| testCSSLength 100
                                , marginRight = CSSOM.marginLength <| testCSSLength 50
                                , marginLeft = CSSOM.marginAuto
                            }
                            default
                            containingDimensions

                    boxModelContent =
                        content boxModel

                    boxModelMargin =
                        margin boxModel
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
                        make
                            { x = 0, y = 0, width = 200, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModel =
                        blockWidth
                            { styles
                                | display = CSSOM.displayBlock
                                , width = CSSOM.widthAuto
                                , marginRight = CSSOM.marginAuto
                                , marginLeft = CSSOM.marginAuto
                            }
                            default
                            containingDimensions

                    boxModelContent =
                        content boxModel

                    boxModelMargin =
                        margin boxModel
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
                        make
                            { x = 0, y = 0, width = 100, height = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }
                            { top = 0, right = 0, bottom = 0, left = 0 }

                    boxModel =
                        blockWidth
                            { styles
                                | display = CSSOM.displayBlock
                                , width = CSSOM.widthAuto
                                , marginRight = CSSOM.marginLength <| testCSSLength 100
                                , marginLeft = CSSOM.marginLength <| testCSSLength 200
                            }
                            BoxModel.default
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
