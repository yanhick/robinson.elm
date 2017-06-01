module LayoutTests exposing (..)

import Box
import BoxModel
import CSSBasicTypes
import CSSOM
import Dict
import Expect
import Layout
import Painting
import Style
import Test exposing (..)


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
        Layout.BlockBox { boxModel } _ ->
            Just boxModel

        Layout.BlockBoxInlineContext { boxModel } _ ->
            Just boxModel


exampleLength =
    Maybe.withDefault CSSBasicTypes.defaultCSSLength (CSSBasicTypes.cssPixelLength 100)


type BoxType
    = Block (List BoxType)
    | Inline (List BoxType)
    | Anonymous (List BoxType)
    | AnonymousInlineRoot (List BoxType)
    | Text


layoutBlockChildren : Test
layoutBlockChildren =
    describe "layout block children"
        [ test "compute the containing block height" <|
            \() ->
                let
                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    getBlockBox children height =
                        Box.BlockContainerBlockContext
                            { styles
                                | display = CSSOM.Block
                                , height = height
                            }
                            children

                    ( laidoutChildren, childrenBoxModel ) =
                        Layout.layoutBlockChildren
                            [ getBlockBox [] <| CSSOM.heightLength exampleLength
                            , getBlockBox [] <| CSSOM.heightLength exampleLength
                            ]
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
                    getStyledElement children height =
                        { styles =
                            { styles
                                | display = CSSOM.Block
                                , height = height
                            }
                        , node = element
                        , children = children
                        }

                    styledRoot =
                        getStyledRoot
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    getStyledNode children height =
                        Style.StyledElement <| getStyledElement children height

                    getStyledRoot children height =
                        Style.StyledRoot <| getStyledElement children height

                    box =
                        Box.boxTree styledRoot

                    (Layout.LayoutRoot { boxModel } _) =
                        Layout.startLayout
                            box
                            containingDimensions

                    boxModelContent =
                        BoxModel.content boxModel
                in
                Expect.equal
                    boxModelContent.height
                    100
        , test "layout itself with padding" <|
            \() ->
                let
                    getStyledElement children height =
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

                    styledRoot =
                        getStyledRoot
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    getStyledNode children height =
                        Style.StyledElement <| getStyledElement children height

                    getStyledRoot children height =
                        Style.StyledRoot <| getStyledElement children height

                    box =
                        Box.boxTree styledRoot

                    (Layout.LayoutRoot { boxModel } _) =
                        Layout.startLayout
                            box
                            containingDimensions

                    boxModelPadding =
                        BoxModel.paddingBox boxModel
                in
                Expect.equal
                    boxModelPadding.height
                    220
        , test "layout itself with borders" <|
            \() ->
                let
                    getStyledElement children height =
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

                    styledRoot =
                        getStyledRoot
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    getStyledNode children height =
                        Style.StyledElement <| getStyledElement children height

                    getStyledRoot children height =
                        Style.StyledRoot <| getStyledElement children height

                    box =
                        Box.boxTree styledRoot

                    (Layout.LayoutRoot { boxModel } _) =
                        Layout.startLayout
                            box
                            containingDimensions

                    boxModelBorder =
                        BoxModel.borderBox boxModel
                in
                Expect.equal
                    boxModelBorder.height
                    220
        , test "layout itself with margins" <|
            \() ->
                let
                    getStyledElement children height =
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

                    getStyledNode children height =
                        Style.StyledElement <| getStyledElement children height

                    getStyledRoot children height =
                        Style.StyledRoot <| getStyledElement children height

                    styledRoot =
                        getStyledRoot
                            [ getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            , getStyledNode [] (CSSOM.heightLength <| testCSSLength 50)
                            ]
                            CSSOM.heightAuto

                    containingDimensions =
                        BoxModel.make
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    box =
                        Box.boxTree styledRoot

                    (Layout.LayoutRoot { boxModel } _) =
                        Layout.startLayout
                            box
                            containingDimensions

                    boxModelMargin =
                        BoxModel.marginBox boxModel
                in
                Expect.equal
                    boxModelMargin.height
                    220
        ]
