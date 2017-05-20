module CSSParser exposing (..)

import CSSBasicTypes exposing (..)
import CSSOM exposing (..)
import CSSSelectors exposing (..)
import Char
import ParseInt exposing (..)
import Parser exposing (..)
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


parseColor : Parser CSSColor
parseColor =
    oneOf
        [ parseColorKeyword
        , parseRGBAColor
        ]


parseRGBAColor : Parser CSSColor
parseRGBAColor =
    let
        parseHex =
            map (parseIntHex >> Result.withDefault 0)
                (keep (Exactly 2) Char.isHexDigit)
    in
    andThen
        (\maybeColor ->
            case maybeColor of
                Just color ->
                    succeed color

                Nothing ->
                    fail "bim"
        )
        (succeed (\r g b -> cssColorFromRGBA { red = r, green = g, blue = b, alpha = 1.0 })
            |. symbol "#"
            |= parseHex
            |= parseHex
            |= parseHex
        )


parseLength : Parser CSSLength
parseLength =
    andThen
        (\maybeLength ->
            case maybeLength of
                Just length ->
                    succeed length

                Nothing ->
                    fail "bim"
        )
        (succeed cssPixelLength
            |= float
            |. keyword "px"
        )


parseColorKeyword : Parser CSSColor
parseColorKeyword =
    andThen
        (\maybeColor ->
            case maybeColor of
                Just color ->
                    succeed color

                Nothing ->
                    fail "bim"
        )
        (succeed cssColorFromColorName
            |= parseIdentifier
        )


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
        , oneOf
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
        , parseBorderWidth "border-left-width" BorderLeftWidth
        , parseBorderWidth "border-right-width" BorderRightWidth
        , parseBorderWidth "border-top-width" BorderTopWidth
        , parseBorderWidth "border-bottom-width" BorderBottomWidth
        , parseHeight
        , parseWidth
        , parseBackgroundColor
        , parseBorderColor "border-top-color" BorderTopColor
        , parseBorderColor "border-bottom-color" BorderBottomColor
        , parseBorderColor "border-left-color" BorderLeftColor
        , parseBorderColor "border-right-color" BorderRightColor
        , parseBorderStyle "border-top-style" BorderTopStyle
        , parseBorderStyle "border-bottom-style" BorderBottomStyle
        , parseBorderStyle "border-left-style" BorderLeftStyle
        , parseBorderStyle "border-right-style" BorderRightStyle
        ]


parseBorderStyle : String -> (CSSBorderStyle SpecifiedValue -> CSSDeclaration) -> Parser CSSDeclaration
parseBorderStyle borderStyleName borderStyleConstructor =
    succeed borderStyleConstructor
        |. keyword borderStyleName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map (always borderStyleNone) (keyword "none")
            , map (always borderStyleSolid) (keyword "solid")
            ]
        |. spaces
        |. symbol ";"


parseBackgroundColor : Parser CSSDeclaration
parseBackgroundColor =
    succeed BackgroundColor
        |. keyword "background-color"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map backgroundColorColor parseColor
            , map (always backgroundColorTransparent) (keyword "transparent")
            ]
        |. spaces
        |. symbol ";"


parseBorderColor : String -> (CSSBorderColor SpecifiedValue -> CSSDeclaration) -> Parser CSSDeclaration
parseBorderColor borderName borderConstructor =
    succeed borderConstructor
        |. keyword borderName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map borderColorColor parseColor
            , map (always borderColorTransparent) (keyword "transparent")
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
            [ map heightLength parseLength
            , map (always heightAuto) (keyword "auto")
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
            [ map widthLength parseLength
            , map (always widthAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parseMargin : String -> (CSSMargin SpecifiedValue -> CSSDeclaration) -> Parser CSSDeclaration
parseMargin marginName marginConstructor =
    succeed marginConstructor
        |. keyword marginName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map marginLength parseLength
            , map (always marginAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parsePadding : String -> (CSSPadding SpecifiedValue -> CSSDeclaration) -> Parser CSSDeclaration
parsePadding paddingName paddingConstructor =
    succeed paddingConstructor
        |. keyword paddingName
        |. spaces
        |. symbol ":"
        |. spaces
        |= map padding parseLength
        |. spaces
        |. symbol ";"


parseBorderWidth : String -> (CSSBorderWidth SpecifiedValue -> CSSDeclaration) -> Parser CSSDeclaration
parseBorderWidth borderWidthName borderWidthConstructor =
    succeed borderWidthConstructor
        |. keyword borderWidthName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map borderWidthLength parseLength
            , map (always borderWidthThin) (keyword "thin")
            , map (always borderWidthMedium) (keyword "medium")
            , map (always borderWidthThick) (keyword "thick")
            ]
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
