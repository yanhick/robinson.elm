module CSSOMTests exposing (..)

import CSSBasicTypes
import CSSOM
import Expect
import Test exposing (..)


smallOffset : CSSOM.CSSOffset CSSOM.SpecifiedValue
smallOffset =
    CSSOM.offsetLength <| testCSSLength 10


testCSSLength length =
    case CSSBasicTypes.cssPixelLength length of
        Just validLength ->
            validLength

        Nothing ->
            CSSBasicTypes.defaultCSSLength


usedOffsets : Test
usedOffsets =
    describe "get used top left right bottom offsets"
        [ test "offsets are 0 if position is not relative" <|
            \() ->
                Expect.equal
                    (CSSOM.usedOffsets CSSOM.Static
                        { top = smallOffset
                        , bottom = smallOffset
                        , left = smallOffset
                        , right = smallOffset
                        }
                    )
                    { x = 0, y = 0 }
        , test "top and left are used if position is relative" <|
            \() ->
                Expect.equal
                    (CSSOM.usedOffsets CSSOM.Relative
                        { top = smallOffset
                        , bottom = CSSOM.offsetAuto
                        , left = smallOffset
                        , right = CSSOM.offsetAuto
                        }
                    )
                    { x = 10, y = 10 }
        , test "bottom and right are used if position is relative" <|
            \() ->
                Expect.equal
                    (CSSOM.usedOffsets CSSOM.Relative
                        { top = CSSOM.offsetAuto
                        , bottom = smallOffset
                        , left = CSSOM.offsetAuto
                        , right = smallOffset
                        }
                    )
                    { x = -10, y = -10 }
        , test "top is used if top and bottom have length" <|
            \() ->
                Expect.equal
                    (CSSOM.usedOffsets CSSOM.Relative
                        { top = smallOffset
                        , bottom = smallOffset
                        , left = CSSOM.offsetAuto
                        , right = CSSOM.offsetAuto
                        }
                    )
                    { x = 0, y = 10 }
        , test "left is used if left and right have length" <|
            \() ->
                Expect.equal
                    (CSSOM.usedOffsets CSSOM.Relative
                        { top = CSSOM.offsetAuto
                        , bottom = CSSOM.offsetAuto
                        , left = smallOffset
                        , right = smallOffset
                        }
                    )
                    { x = 10, y = 0 }
        ]
