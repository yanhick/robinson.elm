module Tests exposing (..)

import AnonymousBoxTests
import BoxModelTests
import CSSBasicTypesTests
import CSSOM
import CSSParser
import CSSTests
import Color
import DOM
import Dict
import Expect
import Fuzz exposing (int, list, string, tuple)
import HtmlParser
import Layout
import LayoutTests
import Parser
import String
import Style
import Test exposing (..)


htmlExample : String
htmlExample =
    """<html>
  <head>
    <title>Test</title>
  </head>
  <div class=" outer ">
    <p class=" inner ">
      Hello, <span id=" name ">world!</span>
    </p>
    <p class=" inner " id=" bye ">
      Goodbye!
    </p>
  </div>
</html>"""


all : Test
all =
    describe "Test Suite"
        [ htmlParser
        , styledTree
        , LayoutTests.layout
        , CSSTests.cssTest
        , CSSBasicTypesTests.cssBasicTypes
        , BoxModelTests.boxModelTests
        , AnonymousBoxTests.anonymousBoxTests
        ]


htmlParser : Test
htmlParser =
    describe "Html parser"
        [ test "parse" <|
            \() ->
                let
                    res =
                        case Parser.run HtmlParser.parse htmlExample of
                            Result.Ok _ ->
                                True

                            Err e ->
                                False
                in
                Expect.true "Expected Html parser to succeed" res
        , test "parse html tag" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case Parser.run HtmlParser.parse "<html></html>" of
                        Ok (DOM.DOMRoot { children, tagName, attributes }) ->
                            List.isEmpty
                                children
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes

                        _ ->
                            False
        , test "parse html tag with newlines" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case Parser.run HtmlParser.parse "<html>\n</html>" of
                        Ok (DOM.DOMRoot { children, tagName, attributes }) ->
                            List.isEmpty
                                children
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes

                        _ ->
                            False
        , test "parse nested tags" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case Parser.run HtmlParser.parse "<html><body></body></html>" of
                        Ok (DOM.DOMRoot { children, tagName, attributes }) ->
                            List.length
                                children
                                == 1
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes
                                && (case children of
                                        [ DOM.Element { children, tagName, attributes } ] ->
                                            List.isEmpty children
                                                && tagName
                                                == "body"

                                        _ ->
                                            False
                                   )

                        _ ->
                            False
        , test "parse text node" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case Parser.run HtmlParser.parse "<html>hello</html>" of
                        Ok (DOM.DOMRoot { children, tagName, attributes }) ->
                            List.length
                                children
                                == 1
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes
                                && (case children of
                                        [ DOM.Text "hello" ] ->
                                            True

                                        _ ->
                                            False
                                   )

                        _ ->
                            False
        , test "parse attributes" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case Parser.run HtmlParser.parse "<html lang=\"us\"></html>" of
                        Ok (DOM.DOMRoot { children, tagName, attributes }) ->
                            List.isEmpty children
                                && attributes
                                == Dict.fromList [ ( "lang", "us" ) ]
                                && tagName
                                == "html"

                        _ ->
                            False
        ]


styledTree : Test
styledTree =
    describe "styled tree"
        [ test "style a dom node" <|
            \() ->
                Expect.true "styled dom node" <|
                    let
                        rootElement =
                            Parser.run HtmlParser.parse "<html class=\"my-class\"></html>"

                        styleSheet =
                            Parser.run CSSParser.parse "html {display:inline;} .my-class {display:block;} .my-other-class {display:inline;} #my-id {display:inline;}"

                        styledTree =
                            Result.map2 Style.styleTree styleSheet rootElement
                    in
                    case styledTree of
                        Ok (Style.StyledRoot { node, styles, children }) ->
                            List.isEmpty children
                                && (case styles of
                                        { display } ->
                                            display
                                                == CSSOM.Block
                                                && (case node of
                                                        { attributes } ->
                                                            attributes == Dict.fromList [ ( "class", "my-class" ) ]
                                                   )
                                   )

                        _ ->
                            False
        , test "style a dom node with multiple selector" <|
            \() ->
                Expect.true "styled dom node" <|
                    let
                        rootElement =
                            Parser.run HtmlParser.parse "<html id=\"my-id\" class=\"my-class\"></html>"

                        styleSheet =
                            Parser.run CSSParser.parse "html.my-class#my-id {display:block;}"

                        styledTree =
                            Result.map2 Style.styleTree styleSheet rootElement
                    in
                    case styledTree of
                        Ok (Style.StyledRoot { node, styles, children }) ->
                            List.isEmpty children
                                && (case styles of
                                        { display } ->
                                            display
                                                == CSSOM.Block
                                                && (case node of
                                                        { attributes } ->
                                                            attributes
                                                                == Dict.fromList
                                                                    [ ( "class", "my-class" )
                                                                    , ( "id", "my-id" )
                                                                    ]
                                                   )
                                   )

                        _ ->
                            False
        ]
