module HtmlParserTests exposing (..)

import DOM
import Dict
import Expect
import HtmlParser
import Parser
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
