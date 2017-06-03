module HtmlParser exposing (parse)

import Char
import DOM
import Dict
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , delayedCommit
        , ignore
        , inContext
        , keep
        , lazy
        , oneOf
        , oneOrMore
        , repeat
        , succeed
        , symbol
        , zeroOrMore
        )


parse : Parser DOM.DOMRoot
parse =
    parseRoot


parseRoot : Parser DOM.DOMRoot
parseRoot =
    inContext "parse element" <|
        succeed
            (\( tagName, attributes ) children ->
                DOM.DOMRoot
                    { children = children
                    , tagName = tagName
                    , attributes = attributes
                    }
            )
            |= parseOpeningTag
            |. spaces
            |= oneOf
                [ parseClosingTag []
                , andThen (\n -> parseNodeList [ n ]) (lazy (\_ -> parseNode))
                ]


parseElement : Parser DOM.DOMNode
parseElement =
    inContext "parse element" <|
        succeed
            (\( tagName, attributes ) children ->
                DOM.Element
                    { children = children
                    , tagName = tagName
                    , attributes = attributes
                    }
            )
            |= parseOpeningTag
            |. spaces
            |= oneOf
                [ parseClosingTag []
                , andThen (\n -> parseNodeList [ n ]) (lazy (\_ -> parseNode))
                ]


parseNodeList : List DOM.DOMNode -> Parser (List DOM.DOMNode)
parseNodeList domNodes =
    oneOf
        [ parseClosingTag domNodes
        , nextNode
            |> andThen (\n -> parseNodeList (domNodes ++ [ n ]))
        ]


nextNode : Parser DOM.DOMNode
nextNode =
    delayedCommit spaces <|
        succeed identity
            |= parseNode


parseClosingTag : a -> Parser a
parseClosingTag after =
    inContext "parse closing tag" <|
        delayedCommit spaces <|
            succeed after
                |. symbol "</"
                |. spaces
                |. parseName
                |. spaces
                |. symbol ">"


parseOpeningTag : Parser ( DOM.TagName, DOM.Attributes )
parseOpeningTag =
    inContext "parse opening tag" <|
        succeed (\tagName attributes -> ( tagName, Dict.fromList attributes ))
            |. spaces
            |. symbol "<"
            |. spaces
            |= parseName
            |. spaces
            |= parseAttributes
            |. spaces
            |. symbol ">"


parseNode : Parser DOM.DOMNode
parseNode =
    inContext "parse node" <|
        oneOf [ parseText, parseElement ]


parseText : Parser DOM.DOMNode
parseText =
    inContext "parse text" <|
        succeed DOM.Text
            |= keep oneOrMore (\c -> c /= '<')


parseAttributes : Parser (List ( String, String ))
parseAttributes =
    inContext "parsing attributes" <|
        repeat zeroOrMore <|
            succeed identity
                |= parseAttribute
                |. spaces


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


parseAttribute : Parser ( String, String )
parseAttribute =
    inContext "parsing attribute" <|
        succeed (\name value -> ( name, value ))
            |= parseName
            |. symbol "="
            |= parseAttributeValue


parseAttributeValue : Parser String
parseAttributeValue =
    inContext "parse attribute value" <|
        succeed identity
            |. oneOf [ symbol "'", symbol "\"" ]
            |= keep zeroOrMore (\c -> c /= '\'' && c /= '"')
            |. oneOf [ symbol "'", symbol "\"" ]


parseName : Parser String
parseName =
    keep zeroOrMore
        (\c ->
            Char.isUpper c
                || Char.isLower c
                || Char.isDigit c
        )
