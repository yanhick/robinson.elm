module Tests exposing (..)

import Layout
import LayoutTests
import Style
import Color
import DOM
import Dict
import Parser
import Test exposing (..)
import HtmlParser
import CSSParser
import CSSOM
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


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


cssExample =
    """* {
  display: block;
}

span {
  display: inline;
}

html {
  width: 600px;
  padding: 10px;
  border-width: 1px;
  margin: auto;
  background: #ffffff;
}

head {
  display: none;
}

.outer {
  background: #00ccff;
  border-color: #666666;
  border-width: 2px;
  margin: 50px;
  padding: 50px;
}

.inner {
  border-color: #cc0000;
  border-width: 4px;
  height: 100px;
  margin-bottom: 20px;
  width: 500px;
}

.inner#bye {
  background: #ffff00;
}

span#name {
  background: red;
  color: white;
}"""


all : Test
all =
    describe "Test Suite"
        [ htmlParser
        , cssParser
        , styledTree
        , LayoutTests.layout
        ]


cssParser : Test
cssParser =
    describe "CSS parser"
        [ test "parse" <|
            \() ->
                let
                    res =
                        case (Parser.run CSSParser.parse cssExample) of
                            Ok _ ->
                                True

                            Err e ->
                                False
                in
                    Expect.true "Expected CSS parser to succeed" res
        , test "parse name selector" <|
            \() ->
                Expect.true "name selector" <|
                    case (Parser.run CSSParser.parse "div {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Just "div"
                                        && List.isEmpty classes
                                        && List.isEmpty ids

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse css with newlines " <|
            \() ->
                Expect.true "name selector" <|
                    case (Parser.run CSSParser.parse "div\n {foo:bar;\n}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Just "div"
                                        && List.isEmpty classes
                                        && List.isEmpty ids

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse id selectors" <|
            \() ->
                Expect.true "id selector" <|
                    case (Parser.run CSSParser.parse "#my-id {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Nothing
                                        && List.isEmpty classes
                                        && ids
                                        == [ "my-id" ]

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse class selectors" <|
            \() ->
                Expect.true "class selector" <|
                    case (Parser.run CSSParser.parse ".my-class {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Nothing
                                        && List.isEmpty ids
                                        && classes
                                        == [ "my-class" ]

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse universal selector" <|
            \() ->
                Expect.true "universal selector" <|
                    case (Parser.run CSSParser.parse "*{foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Universal ] ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse multiple classes" <|
            \() ->
                Expect.true "multiple classes" <|
                    case (Parser.run CSSParser.parse ".my-class.my-other-class {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Nothing
                                        && List.isEmpty ids
                                        && classes
                                        == [ "my-other-class", "my-class" ]

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse simple selector" <|
            \() ->
                Expect.true "simple selector" <|
                    case (Parser.run CSSParser.parse "div.my-class#my-id {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple { tag, classes, ids } ] ->
                                    tag
                                        == Just "div"
                                        && ids
                                        == [ "my-id" ]
                                        && classes
                                        == [ "my-class" ]

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse keyword value" <|
            \() ->
                Expect.true "keyword value" <|
                    case (Parser.run CSSParser.parse "div {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ { name, value } ] ->
                                    name
                                        == "foo"
                                        && case value of
                                            CSSOM.Keyword "bar" ->
                                                True

                                            _ ->
                                                False

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse length value" <|
            \() ->
                Expect.true "length value" <|
                    case (Parser.run CSSParser.parse "div {foo:10px;}") of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ { name, value } ] ->
                                    name
                                        == "foo"
                                        && case value of
                                            CSSOM.Length 10 CSSOM.Pixel ->
                                                True

                                            _ ->
                                                False

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse color value" <|
            \() ->
                Expect.true "color value" <|
                    case (Parser.run CSSParser.parse "div {foo:#CCFF00;}") of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ { name, value } ] ->
                                    name
                                        == "foo"
                                        && case value of
                                            CSSOM.ColorValue color ->
                                                Color.toRgb color
                                                    == { red = 204
                                                       , green = 255
                                                       , blue = 0
                                                       , alpha = 1.0
                                                       }

                                            _ ->
                                                False

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse multiple selectors" <|
            \() ->
                Expect.true "multiple selectors" <|
                    case (Parser.run CSSParser.parse "div, p {foo:bar;}") of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSOM.Simple first, CSSOM.Simple second ] ->
                                    first.tag
                                        == Just "p"
                                        && second.tag
                                        == Just "div"

                                _ ->
                                    False

                        _ ->
                            False
        ]


htmlParser : Test
htmlParser =
    describe "Html parser"
        [ test "parse" <|
            \() ->
                let
                    res =
                        case (Parser.run HtmlParser.parse htmlExample) of
                            Result.Ok _ ->
                                True

                            Err e ->
                                False
                in
                    Expect.true "Expected Html parser to succeed" res
        , test "parse html tag" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case (Parser.run HtmlParser.parse "<html></html>") of
                        Ok (DOM.Element { children, tagName, attributes }) ->
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
                    case (Parser.run HtmlParser.parse "<html>\n</html>") of
                        Ok (DOM.Element { children, tagName, attributes }) ->
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
                    case (Parser.run HtmlParser.parse "<html><body></body></html>") of
                        Ok (DOM.Element { children, tagName, attributes }) ->
                            List.length
                                children
                                == 1
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes
                                && case children of
                                    [ DOM.Element { children, tagName, attributes } ] ->
                                        List.isEmpty children
                                            && tagName
                                            == "body"

                                    _ ->
                                        False

                        _ ->
                            False
        , test "parse text node" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case (Parser.run HtmlParser.parse "<html>hello</html>") of
                        Ok (DOM.Element { children, tagName, attributes }) ->
                            List.length
                                children
                                == 1
                                && tagName
                                == "html"
                                && Dict.isEmpty attributes
                                && case children of
                                    [ DOM.Text "hello" ] ->
                                        True

                                    _ ->
                                        False

                        _ ->
                            False
        , test "parse attributes" <|
            \() ->
                Expect.true "Expect a dom node" <|
                    case (Parser.run HtmlParser.parse "<html lang=\"us\"></html>") of
                        Ok (DOM.Element { children, tagName, attributes }) ->
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
                            Parser.run CSSParser.parse "html {foo:bar;} .my-class {display:block;} .my-other-class {display:inline;} #my-id {display:inline;}"

                        styledTree =
                            Result.map2 Style.styleTree styleSheet rootElement
                    in
                        case styledTree of
                            Ok (Style.StyledElement { node, styles, children }) ->
                                List.isEmpty children
                                    && case styles of
                                        { display } ->
                                            display
                                                == Style.Block
                                                && case node of
                                                    { attributes } ->
                                                        attributes == Dict.fromList [ ( "class", "my-class" ) ]

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
                            Ok (Style.StyledElement { node, styles, children }) ->
                                List.isEmpty children
                                    && case styles of
                                        { display } ->
                                            display
                                                == Style.Block
                                                && case node of
                                                    { attributes } ->
                                                        attributes
                                                            == Dict.fromList
                                                                [ ( "class", "my-class" )
                                                                , ( "id", "my-id" )
                                                                ]

                            _ ->
                                False
        ]
