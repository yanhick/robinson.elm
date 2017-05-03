module CSSBasicTypesTests exposing (..)

import CSSBasicTypes exposing (..)
import Test exposing (..)
import Expect


cssBasicTypes : Test
cssBasicTypes =
    describe "CSS basic types"
        [ length, color ]


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing =
    not << isJust


length : Test
length =
    describe "CSS Length"
        [ test "create CSSLength value" <|
            \() ->
                Expect.true "valid length" <|
                    isJust (cssLength 1 Pixel)
        , test "compute pixel length" <|
            \() ->
                Expect.equal (Just 1)
                    (Maybe.map computedCSSLength <|
                        cssLength 1 Pixel
                    )
        ]


color : Test
color =
    describe "CSS color"
        [ test "create CSSColor value from RGBA" <|
            \() ->
                Expect.true "valid color" <|
                    isJust
                        (cssColorFromRGBA
                            { red = 10
                            , green = 10
                            , blue = 10
                            , alpha = 0.5
                            }
                        )
        , test "create CSSColor value from name" <|
            \() ->
                Expect.true "valid color" <|
                    isJust
                        (cssColorFromColorName "red")
        , test "create no CSSColor value for invalid name" <|
            \() ->
                Expect.true "no color" <|
                    isNothing
                        (cssColorFromColorName "invalid color name")
        , test "compute a CSSColor into an RGBA value" <|
            \() ->
                Expect.equal
                    (Maybe.map computedCSSColor <| cssColorFromColorName "red")
                    (Just { red = 255, green = 0, blue = 0, alpha = 1.0 })
        ]
