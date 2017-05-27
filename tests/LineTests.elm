module LineTests exposing (..)

import Box
import Expect
import Line exposing (..)
import Style
import Test exposing (..)


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
                    (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 50 ]))
        , test "get root line box with container " <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineContainer Style.initialStyles [] ]))
                    (LineBoxRoot (LineBoxContainer [ LineBoxContainer [] ]))
        , test "get root line box with container and text " <|
            \() ->
                Expect.equal
                    (lineBoxRoot (Box.InlineBoxRoot Style.initialStyles [ Box.InlineContainer Style.initialStyles [], Box.InlineText "hello" ]))
                    (LineBoxRoot (LineBoxContainer [ LineBoxContainer [], LineBoxText "hello" 50 ]))
        ]


testGetLines : Test
testGetLines =
    describe "get lines"
        [ test "gets a single line if there are no elements" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [])) 100)
                    [ LineBoxRoot (LineBoxContainer []) ]
        , test "gets a single line with text" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 50 ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 50 ]) ]
        , test "gets a single line with text overflowing" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 150 ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 150 ]) ]
        , test "gets a multiple lines to fit the text" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80, LineBoxText "world" 30 ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80 ]), LineBoxRoot (LineBoxContainer [ LineBoxText "world" 30 ]) ]
        , test "gets one line with text that fit exactly" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80, LineBoxText "world" 20 ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80, LineBoxText "world" 20 ]) ]
        , test "gets multple lines with multiple overflowing text" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80, LineBoxText "world" 60 ])) 50)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80 ]), LineBoxRoot (LineBoxContainer [ LineBoxText "world" 60 ]) ]
        , test "gets multiple lines and preserve the child containers" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80, LineBoxContainer [ LineBoxText "world" 60 ] ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxText "hello" 80 ]), LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "world" 60 ] ]) ]
        , test "gets multiple lines and preserve the sibling containers" <|
            \() ->
                Expect.equal
                    (getLines (LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "hello" 80 ], LineBoxContainer [ LineBoxText "world" 60 ] ])) 100)
                    [ LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "hello" 80 ] ]), LineBoxRoot (LineBoxContainer [ LineBoxContainer [ LineBoxText "world" 60 ] ]) ]
        ]
