module CSSParser exposing (..)

import Char
import ParseInt exposing (..)
import Parser exposing (..)
import CSSOM exposing (..)
import Color exposing (..)
import String


parse : Parser CSSStyleSheet
parse =
    parseCSSStyleSheet


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
        |. spaces
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


type IdOrClass
    = Id String
    | Class String


parseSimpleSelector : Parser CSSSelector
parseSimpleSelector =
    let
        tagSelector =
            parseIdentifier

        classSelector =
            succeed identity
                |. symbol "."
                |= parseIdentifier

        idSelector =
            succeed identity
                |. symbol "#"
                |= parseIdentifier

        universalSelector =
            map (always Universal) (symbol "*")

        classOrIdSelectors =
            repeat zeroOrMore
                (oneOf
                    [ map Id idSelector
                    , map Class classSelector
                    ]
                )

        separate =
            List.foldl
                (\selector ( ids, classes ) ->
                    case selector of
                        Id s ->
                            ( s :: ids, classes )

                        Class s ->
                            ( ids, s :: classes )
                )
                ( [], [] )
    in
        oneOf
            [ universalSelector
            , (oneOf
                [ succeed
                    (\tag ( ids, classes ) ->
                        Simple
                            { tag = Just tag
                            , classes = classes
                            , ids = ids
                            }
                    )
                    |= tagSelector
                    |= map separate classOrIdSelectors
                , succeed
                    (\( ids, classes ) ->
                        Simple
                            { tag = Nothing
                            , classes = classes
                            , ids = ids
                            }
                    )
                    |= map separate classOrIdSelectors
                ]
              )
            ]


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
        |. symbol ":"
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
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')
