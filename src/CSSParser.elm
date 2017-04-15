module CSSParser exposing (..)

import Char
import ParseInt exposing (..)
import Parser exposing (..)
import CSSOM exposing (..)
import Color exposing (..)
import String


parseCSSStyleSheet : Parser CSSStyleSheet
parseCSSStyleSheet =
    repeat oneOrMore
        (succeed identity
            |. spaces
            |= parseRule
            |. spaces
        )


parseRule : Parser CSSRule
parseRule =
    succeed (\s d -> { selectors = s, declarations = d })
        |= parseSelectors
        |= parseDeclarations


parseUnit : Parser CSSUnit
parseUnit =
    succeed Pixel
        |. symbol "px"


parseValue : Parser CSSValue
parseValue =
    oneOf [ parseLength, parseColor, parseKeyword ]


parseColor : Parser CSSValue
parseColor =
    let
        parseHex =
            map (parseIntHex >> Result.withDefault 0)
                (keep (Exactly 2) Char.isHexDigit)
    in
        succeed (\r g b -> ColorValue (Color.rgb r g b))
            |. symbol "#"
            |= parseHex
            |= parseHex
            |= parseHex


parseLength : Parser CSSValue
parseLength =
    succeed Length
        |= float
        |= parseUnit


parseKeyword : Parser CSSValue
parseKeyword =
    succeed Keyword
        |= parseIdentifier


parseSimpleSelector : Parser CSSSelector
parseSimpleSelector =
    let
        tagSelector =
            map Tag parseIdentifier

        classSelector =
            succeed Class
                |. symbol "."
                |= parseIdentifier

        idSelector =
            succeed Id
                |. symbol "#"
                |= parseIdentifier

        universalSelector =
            map (always Universal) (symbol "*")
    in
        succeed Simple
            |= repeat oneOrMore
                (oneOf
                    [ universalSelector
                    , idSelector
                    , classSelector
                    , tagSelector
                    ]
                )


parseSelectors : Parser (List CSSSelector)
parseSelectors =
    let
        selectorListHelp selectors =
            oneOf
                [ nextSelector
                    |> andThen (\s -> selectorListHelp (s :: selectors))
                , succeed selectors
                ]

        nextSelector =
            delayedCommit spaces <|
                succeed identity
                    |. symbol ","
                    |. spaces
                    |= parseSimpleSelector
    in
        succeed identity
            |= andThen (\s -> selectorListHelp [ s ]) parseSimpleSelector


parseDeclaration : Parser CSSDeclaration
parseDeclaration =
    succeed (\n v -> { name = n, value = v })
        |= parseIdentifier
        |. spaces
        |. symbol "="
        |. spaces
        |= parseValue
        |. spaces
        |. symbol ";"


parseDeclarations : Parser (List CSSDeclaration)
parseDeclarations =
    succeed identity
        |. symbol "{"
        |. spaces
        |= repeat oneOrMore
            (succeed identity
                |. spaces
                |= parseDeclaration
                |. spaces
            )
        |. symbol "}"


parseIdentifier : Parser String
parseIdentifier =
    keep oneOrMore
        (\c ->
            Char.isDigit c || Char.isLower c || c == '-' || c == '_'
        )


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
