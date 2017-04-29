module CSSTests exposing (..)

import Test exposing (..)
import CSSOM
import CSSParser
import Parser
import Expect
import Color


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


cssTest : Test
cssTest =
    describe "css tests"
        [ cssParser
        , passingTests
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


passingTestCases : List ( String, String )
passingTestCases =
    [ ( "declaration without properties", "div{}" )
    ]


passingTests : Test
passingTests =
    let
        isOk css =
            case (Parser.run CSSParser.parse css) of
                Ok _ ->
                    True

                _ ->
                    False
    in
        describe "those should pass"
            (List.map
                (\( description, css ) ->
                    test description
                        (\() -> Expect.true description (isOk css))
                )
                passingTestCases
            )
