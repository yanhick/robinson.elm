module LineTests exposing (..)

import Box
import Expect
import Line exposing (..)
import Style
import Test exposing (..)


testLayoutLineBoxRoot : Test
testLayoutLineBoxRoot =
    describe "layout a line"
        [ test "layout with text line box" <|
            \() ->
                Expect.equal
                    (layoutLineBoxRoot (LineBoxRoot (LineBoxText "hello" { width = 100, height = 20 })))
                    (LayoutLineBoxRoot { width = 100, height = 20 } (LayoutLineBoxText "hello" { x = 0, y = 0, width = 100, height = 20 }))
        , test "layout with multiple text line box" <|
            \() ->
                Expect.equal
                    (layoutLineBoxRoot
                        (LineBoxRoot
                            (LineBoxContainer
                                [ LineBoxText "hello" { width = 100, height = 20 }
                                , LineBoxText "world" { width = 150, height = 30 }
                                ]
                            )
                        )
                    )
                    (LayoutLineBoxRoot { width = 250, height = 30 }
                        (LayoutLineBoxContainer { x = 0, y = 0, width = 250, height = 30 }
                            [ LayoutLineBoxText "hello" { x = 0, y = 0, width = 100, height = 20 }
                            , LayoutLineBoxText "world" { x = 100, y = 0, width = 150, height = 30 }
                            ]
                        )
                    )
        , test "layout with multiple text line box and containers" <|
            \() ->
                Expect.equal
                    (layoutLineBoxRoot
                        (LineBoxRoot
                            (LineBoxContainer
                                [ LineBoxText "hello" { width = 100, height = 20 }
                                , LineBoxContainer
                                    [ LineBoxText "world" { width = 120, height = 30 }
                                    ]
                                ]
                            )
                        )
                    )
                    (LayoutLineBoxRoot { width = 220, height = 30 }
                        (LayoutLineBoxContainer { x = 0, y = 0, width = 220, height = 30 }
                            [ LayoutLineBoxText "hello" { x = 0, y = 0, width = 100, height = 20 }
                            , LayoutLineBoxContainer { x = 100, y = 0, width = 120, height = 30 }
                                [ LayoutLineBoxText "world" { x = 100, y = 0, width = 120, height = 30 }
                                ]
                            ]
                        )
                    )
        ]


testLineBoxRoot : Test
testLineBoxRoot =
    describe "get line box tree"
        [ test "get root line box tree" <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles []))
                    (LineBoxRoot (LineBoxContainer []))
        , test "get root line box with text" <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineText "hello" ]))
                    (LineBoxRoot
                        (LineBoxContainer
                            [ LineBoxText "hello" { width = 50, height = 20 }
                            ]
                        )
                    )
        , test "get root line box with container " <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineContainer Style.initialStyles [] ]))
                    (LineBoxRoot (LineBoxContainer [ LineBoxContainer [] ]))
        , test "get root line box with container and text " <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineContainer Style.initialStyles [], Box.InlineText "hello" ]))
                    (LineBoxRoot
                        (LineBoxContainer
                            [ LineBoxContainer []
                            , LineBoxText "hello" { width = 50, height = 20 }
                            ]
                        )
                    )
        , test "get root line box with split text" <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineContainer Style.initialStyles [], Box.InlineText "hello world" ]))
                    (LineBoxRoot
                        (LineBoxContainer
                            [ LineBoxContainer []
                            , LineBoxText "hello" { width = 50, height = 20 }
                            , LineBoxText " " { width = 10, height = 20 }
                            , LineBoxText "world" { width = 50, height = 20 }
                            ]
                        )
                    )
        ]


testGetLines : Test
testGetLines =
    describe "get lines"
        [ test "gets a single line if there are no elements" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [])))
                    [ LineBoxRoot (LineBoxContainer []) ]
        , test "gets a single line with text" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 50, height = 20 } ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 50, height = 20 } ]) ]
        , test "gets a single line with text overflowing" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 150, height = 20 } ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 150, height = 20 } ]) ]
        , test "gets a multiple lines to fit the text" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 }, LineBoxText "world" { width = 30, height = 20 } ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 } ]), LineBoxRoot (LineBoxContainer [ LineBoxText "world" { width = 30, height = 20 } ]) ]
        , test "gets one line with text that fit exactly" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 }, LineBoxText "world" { width = 20, height = 20 } ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 }, LineBoxText "world" { width = 20, height = 20 } ]) ]
        , test "gets multple lines with multiple overflowing text" <|
            \() ->
                Expect.equal
                    (getLines
                        50
                        (LineBoxRoot
                            (LineBoxContainer
                                [ LineBoxText "hello" { width = 80, height = 20 }
                                , LineBoxText "world" { width = 60, height = 20 }
                                ]
                            )
                        )
                    )
                    [ LineBoxRoot
                        (LineBoxContainer
                            [ LineBoxText "hello" { width = 80, height = 20 }
                            ]
                        )
                    , LineBoxRoot
                        (LineBoxContainer
                            [ LineBoxText "world" { width = 60, height = 20 }
                            ]
                        )
                    ]
        , test "gets multiple lines and preserve the child containers" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 }, LineBoxContainer [ LineBoxText "world" { width = 60, height = 20 } ] ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 } ]), LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "world" { width = 60, height = 20 } ] ]) ]
        , test "gets multiple lines and preserve the sibling containers" <|
            \() ->
                Expect.equal
                    (getLines 100 (LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 } ], LineBoxContainer [ LineBoxText "world" { width = 60, height = 20 } ] ])))
                    [ LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "hello" { width = 80, height = 20 } ] ]), LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "world" { width = 60, height = 20 } ] ]) ]
        ]


testMeasureText : Test
testMeasureText =
    describe "measure text"
        [ test "measure width and height of text string" <|
            \() ->
                Expect.equal
                    (measureText "hello")
                    { width = 50, height = 20 }
        ]


testStackLineBoxes : Test
testStackLineBoxes =
    describe "stack line boxes"
        [ test "stack line boxes to get x,y position" <|
            \() ->
                Expect.equal
                    (stackLineBoxes { x = 10, y = 30 }
                        [ LayoutLineBoxRoot
                            { height = 30, width = 100 }
                            (LayoutLineBoxContainer { x = 0, y = 0, height = 0, width = 0 } [])
                        , LayoutLineBoxRoot
                            { height = 50, width = 100 }
                            (LayoutLineBoxContainer { x = 0, y = 0, height = 0, width = 0 } [])
                        ]
                    )
                    [ StackedLayoutLineBoxRoot { x = 10, y = 30 }
                        (LayoutLineBoxRoot
                            { height = 30, width = 100 }
                            (LayoutLineBoxContainer { x = 0, y = 0, height = 0, width = 0 } [])
                        )
                    , StackedLayoutLineBoxRoot { x = 10, y = 60 }
                        (LayoutLineBoxRoot
                            { height = 50, width = 100 }
                            (LayoutLineBoxContainer { x = 0, y = 0, height = 0, width = 0 } [])
                        )
                    ]
        ]
