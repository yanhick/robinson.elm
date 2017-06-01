module StyleTests exposing (..)

import CSSOM
import CSSParser
import Dict
import Expect
import HtmlParser
import Parser
import Style
import Test exposing (..)


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
