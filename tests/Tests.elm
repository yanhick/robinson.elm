module Tests exposing (..)

import Parser
import Test exposing (..)
import HtmlParser
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
        ]
