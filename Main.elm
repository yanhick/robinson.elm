module Main exposing (..)

import Html exposing (..)
import HtmlParser
import Parser
import DOM


main =
    let
        res =
            Parser.run HtmlParser.parse "<div></div>"

        toString e =
            case e of
                DOM.Element { tagName } ->
                    tagName

                DOM.Text t ->
                    t
    in
        case res of
            Ok html ->
                text (toString html)

            Err _ ->
                text "oups"
