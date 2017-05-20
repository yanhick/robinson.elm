module BoxModelTests exposing (..)

import BoxModel exposing (..)
import Expect
import Fuzz exposing (..)
import Test exposing (..)


box : BoxModel
box =
    boxModel
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
        boxModel
        fuzzRect
        fuzzEdgeSize
        fuzzEdgeSize
        fuzzEdgeSize


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
