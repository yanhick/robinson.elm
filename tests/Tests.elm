module Tests exposing (..)

import Parser
import Test exposing (..)
import HtmlParser
import CSSParser
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


cssExample2 =
    """* {
  display: block;
}
"""


all : Test
all =
    describe "Test Suite"
        [ describe "Html parser"
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
            ]
        , describe "CSS parser"
            [ test "parse" <|
                \() ->
                    let
                        res =
                            case (Parser.run CSSParser.parse cssExample) of
                                Result.Ok _ ->
                                    True

                                Err e ->
                                    False
                    in
                        Expect.true "Expected CSS parser to succeed" res
            ]
        ]
