module CSSTests exposing (..)

import CSSBasicTypes
import CSSOM
import CSSParser
import CSSSelectors
import Expect
import Parser
import Test exposing (..)


cssExample =
    """* {
  display: block;
}

span {
  display: inline;
}

html {
  width: 600px;
  padding-top: 10px;
  margin-left: auto;
  background-color: #ffffff;
}

head {
  display: none;
}

.outer {
  background-color: #00ccff;
  margin-left: 50px;
  padding-top: 50px;
}

.inner {
  height: 100px;
  margin-bottom: 20px;
  width: 500px;
}

.inner#bye {
  background-color: #ffff00;
}

span#name {
  background-color: red;
}"""


cssParser : Test
cssParser =
    describe "CSS parser"
        [ test "parse" <|
            \() ->
                let
                    res =
                        case Parser.run CSSParser.parse cssExample of
                            Ok _ ->
                                True

                            Err e ->
                                False
                in
                Expect.true "Expected CSS parser to succeed" res
        , test "parse name selector" <|
            \() ->
                Expect.true "name selector" <|
                    case Parser.run CSSParser.parse "div {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse "div\n {display:block;\n}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse "#my-id {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse ".my-class {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse "*{display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Universal ] ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse multiple classes" <|
            \() ->
                Expect.true "multiple classes" <|
                    case Parser.run CSSParser.parse ".my-class.my-other-class {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse "div.my-class#my-id {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple { tag, classes, ids } ] ->
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
                    case Parser.run CSSParser.parse "div {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.Display CSSOM.Block ] ->
                                    True

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse length value" <|
            \() ->
                Expect.true "length value" <|
                    case Parser.run CSSParser.parse "div {margin-left:10px;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.MarginLeft margin ] ->
                                    (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 10

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse color value" <|
            \() ->
                Expect.true "color value" <|
                    case Parser.run CSSParser.parse "div {background-color:#CCFF00;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.BackgroundColor color ] ->
                                    CSSOM.usedBackgroundColor color
                                        == { red = 204
                                           , green = 255
                                           , blue = 0
                                           , alpha = 1.0
                                           }

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse multiple selectors" <|
            \() ->
                Expect.true "multiple selectors" <|
                    case Parser.run CSSParser.parse "div, p {display:block;}" of
                        Ok [ { selectors, declarations } ] ->
                            case selectors of
                                [ CSSSelectors.Simple first, CSSSelectors.Simple second ] ->
                                    first.tag
                                        == Just "p"
                                        && second.tag
                                        == Just "div"

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse margin all shorthands" <|
            \() ->
                Expect.true "margin all shorthand" <|
                    case Parser.run CSSParser.parse "div {margin:10px;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.MarginTop marginTop, CSSOM.MarginRight marginRight, CSSOM.MarginBottom marginBottom, CSSOM.MarginLeft marginLeft ] ->
                                    let
                                        is10px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 10
                                    in
                                    is10px marginLeft && is10px marginRight && is10px marginTop && is10px marginBottom

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse margin top and bottom shorthands" <|
            \() ->
                Expect.true "margin top and bottom shorthand" <|
                    case Parser.run CSSParser.parse "div {margin:20px 10px;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.MarginTop marginTop, CSSOM.MarginRight marginRight, CSSOM.MarginBottom marginBottom, CSSOM.MarginLeft marginLeft ] ->
                                    let
                                        is10px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 10

                                        is20px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 20
                                    in
                                    is10px
                                        marginLeft
                                        && is10px marginRight
                                        && is20px marginTop
                                        && is20px marginBottom

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse margin top, left/right and bottom shorthands" <|
            \() ->
                Expect.true "margin top left/right and bottom shorthand" <|
                    case Parser.run CSSParser.parse "div {margin:20px 30px 10px;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.MarginTop marginTop, CSSOM.MarginRight marginRight, CSSOM.MarginBottom marginBottom, CSSOM.MarginLeft marginLeft ] ->
                                    let
                                        is10px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 10

                                        is20px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 20

                                        is30px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 30
                                    in
                                    is30px
                                        marginLeft
                                        && is30px marginRight
                                        && is20px marginTop
                                        && is10px marginBottom

                                _ ->
                                    False

                        _ ->
                            False
        , test "parse margin top, left, right and bottom shorthands" <|
            \() ->
                Expect.true "margin top left right and bottom shorthand" <|
                    case Parser.run CSSParser.parse "div {margin:20px 30px 10px 40px;}" of
                        Ok [ { selectors, declarations } ] ->
                            case declarations of
                                [ CSSOM.MarginTop marginTop, CSSOM.MarginRight marginRight, CSSOM.MarginBottom marginBottom, CSSOM.MarginLeft marginLeft ] ->
                                    let
                                        is10px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 10

                                        is20px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 20

                                        is30px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 30

                                        is40px margin =
                                            (Maybe.withDefault 0 <| CSSOM.usedMargin <| CSSOM.computedMargin margin) == 40
                                    in
                                    is40px
                                        marginLeft
                                        && is30px marginRight
                                        && is20px marginTop
                                        && is10px marginBottom

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
            case Parser.run CSSParser.parse css of
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
