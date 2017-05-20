module HtmlParser exposing (..)

import Char
import DOM exposing (..)
import Dict
import Parser exposing (..)


parse : Parser DOMRoot
parse =
    parseRoot


parseRoot : Parser DOMRoot
parseRoot =
    inContext "parse element" <|
        succeed
            (\( tagName, attributes ) children ->
                DOMRoot
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


parseElement : Parser DOMNode
parseElement =
    inContext "parse element" <|
        succeed
            (\( tagName, attributes ) children ->
                Element
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


parseNodeList : List DOMNode -> Parser (List DOMNode)
parseNodeList domNodes =
    oneOf
        [ parseClosingTag domNodes
        , nextNode
            |> andThen (\n -> parseNodeList (domNodes ++ [ n ]))
        ]


nextNode : Parser DOMNode
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


parseOpeningTag : Parser ( TagName, Attributes )
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


parseNode : Parser DOMNode
parseNode =
    inContext "parse node" <|
        oneOf [ parseText, parseElement ]


parseText : Parser DOMNode
parseText =
    inContext "parse text" <|
        succeed Text
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
