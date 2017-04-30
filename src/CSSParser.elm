module CSSParser exposing (parse)

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


parseColor : Parser CSSColor
parseColor =
    let
        parseHex =
            map (parseIntHex >> Result.withDefault 0)
                (keep (Exactly 2) Char.isHexDigit)
    in
        oneOf
            [ map ColorKeyword parseColorKeyword
            , succeed (\r g b -> RGBA (Color.rgb r g b))
                |. symbol "#"
                |= parseHex
                |= parseHex
                |= parseHex
            ]


parseLength : Parser CSSLength
parseLength =
    succeed CSSLength
        |= float
        |= parseUnit


parseColorKeyword : Parser CSSColorKeyword
parseColorKeyword =
    succeed identity
        |= oneOf
            [ map (\_ -> Red) (keyword "red")
            , map (\_ -> White) (keyword "white")
            ]


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
    oneOf
        [ parseDisplay
        , parseMargin "margin-left" MarginLeft
        , parseMargin "margin-right" MarginRight
        , parseMargin "margin-top" MarginTop
        , parseMargin "margin-bottom" MarginBottom
        , parsePadding "padding-left" PaddingLeft
        , parsePadding "padding-right" PaddingRight
        , parsePadding "padding-top" PaddingTop
        , parsePadding "padding-bottom" PaddingBottom
        , parseHeight
        , parseWidth
        , parseBackgroundColor
        ]


parseBackgroundColor : Parser CSSDeclaration
parseBackgroundColor =
    succeed BackgroundColor
        |. keyword "background-color"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map BackgroundColorColor parseColor
            , map (always BackgroundColorTransparent) (keyword "transparent")
            ]
        |. spaces
        |. symbol ";"


parseHeight : Parser CSSDeclaration
parseHeight =
    succeed Height
        |. keyword "height"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map HeightLength parseLength
            , map (always HeightAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parseWidth : Parser CSSDeclaration
parseWidth =
    succeed Width
        |. keyword "width"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map WidthLength parseLength
            , map (always WidthAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parseMargin : String -> (CSSMargin -> CSSDeclaration) -> Parser CSSDeclaration
parseMargin marginName marginConstructor =
    succeed marginConstructor
        |. keyword marginName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map MarginLength parseLength
            , map (always MarginAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parsePadding : String -> (CSSPadding -> CSSDeclaration) -> Parser CSSDeclaration
parsePadding paddingName paddingConstructor =
    succeed paddingConstructor
        |. keyword paddingName
        |. spaces
        |. symbol ":"
        |. spaces
        |= map PaddingLength parseLength
        |. spaces
        |. symbol ";"


parseDisplay : Parser CSSDeclaration
parseDisplay =
    succeed Display
        |. keyword "display"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map (always Block)
                (keyword "block")
            , map
                (always Inline)
                (keyword "inline")
            , map
                (always None)
                (keyword "none")
            ]
        |. spaces
        |. symbol ";"


parseDeclarations : Parser (List CSSDeclaration)
parseDeclarations =
    succeed identity
        |. symbol "{"
        |. spaces
        |= repeat zeroOrMore
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
