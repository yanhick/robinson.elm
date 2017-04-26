module LayoutTests exposing (..)

import Dict
import CSSOM
import Style
import Layout
import Test exposing (..)
import Expect


layout : Test
layout =
    describe "layout"
        [ calculateBlockWidth
        , calculateBlockHeight
        , calculateBlockPosition
        ]


calculateBlockWidth : Test
calculateBlockWidth =
    describe "calculate block width"
        [ test "set the block width with an explicit width" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Length 50 CSSOM.Pixel
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.equal laidoutDimensions.content.width 50
        , test "set the block width with an auto width" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.equal laidoutDimensions.content.width 200
        , test "set the margins width with auto margins and explicit width" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Length 100 CSSOM.Pixel
                                    , marginLeft = Style.Auto
                                    , marginRight = Style.Auto
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 100
                            && laidoutDimensions.margin.left
                            == 50
                            && laidoutDimensions.margin.right
                            == 50
                        )
        , test "set the margins width with auto margins and explicit width" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Length 100 CSSOM.Pixel
                                    , marginLeft = Style.Auto
                                    , marginRight = Style.Auto
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 100
                            && laidoutDimensions.margin.left
                            == 50
                            && laidoutDimensions.margin.right
                            == 50
                        )
        , test "resize left auto margin when right margin and width length is explicit" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Length 100 CSSOM.Pixel
                                    , marginLeft = Style.Length 50 CSSOM.Pixel
                                    , marginRight = Style.Auto
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 100
                            && laidoutDimensions.margin.left
                            == 50
                            && laidoutDimensions.margin.right
                            == -50
                        )
        , test "resize right auto margin when left margin and width length is explicit" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Length 100 CSSOM.Pixel
                                    , marginRight = Style.Length 50 CSSOM.Pixel
                                    , marginLeft = Style.Auto
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 100
                            && laidoutDimensions.margin.left
                            == 50
                            && laidoutDimensions.margin.right
                            == 50
                        )
        , test "set auto margins to 0 if width is auto" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 200, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Auto
                                    , marginRight = Style.Auto
                                    , marginLeft = Style.Auto
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 200
                            && laidoutDimensions.margin.left
                            == 0
                            && laidoutDimensions.margin.right
                            == 0
                        )
        , test "make right margin negative if the width was going to be negative" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    containingDimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 100, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
                        Layout.calculateBlockWidth
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , width = Style.Auto
                                    , marginRight = Style.Length 100 CSSOM.Pixel
                                    , marginLeft = Style.Length 200 CSSOM.Pixel
                                }
                             , children = []
                             }
                            )
                            dimensions
                            containingDimensions
                in
                    Expect.true ""
                        (laidoutDimensions.content.width
                            == 0
                            && laidoutDimensions.margin.left
                            == 200
                            && laidoutDimensions.margin.right
                            == -100
                        )
        ]


calculateBlockHeight : Test
calculateBlockHeight =
    describe "calculate block height"
        [ test "set the block height if provided" <|
            \() ->
                let
                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    styles =
                        Style.initialStyles

                    height =
                        Layout.calculateBlockHeight
                            ({ node = element
                             , styles =
                                { styles
                                    | display = Style.Block
                                    , height = Style.Length 50 CSSOM.Pixel
                                }
                             , children = []
                             }
                            )
                in
                    Expect.equal height 50
        , test "get 0 if auto height" <|
            \() ->
                let
                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    height =
                        Layout.calculateBlockHeight
                            ({ node = element
                             , styles =
                                Style.initialStyles
                             , children = []
                             }
                            )
                in
                    Expect.equal height 0
        ]


calculateBlockPosition : Test
calculateBlockPosition =
    describe "calculate block position"
        [ test "set the block position" <|
            \() ->
                let
                    edgeSize =
                        { top = 0, right = 0, bottom = 0, left = 0 }

                    element =
                        { tagName = "div"
                        , attributes = Dict.fromList [ ( "foo", "bar" ) ]
                        , children = []
                        }

                    dimensions =
                        Layout.dimensions
                            { x = 0, y = 0, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    containingDimensions =
                        Layout.dimensions
                            { x = 10, y = 20, width = 0, height = 0 }
                            edgeSize
                            edgeSize
                            edgeSize

                    laidoutDimensions =
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
                        (laidoutDimensions.content.x
                            == 10
                            && laidoutDimensions.content.y
                            == 20
                        )
        ]
