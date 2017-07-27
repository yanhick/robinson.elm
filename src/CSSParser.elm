module CSSParser exposing (parse)

import CSSBasicTypes
import CSSOM
import CSSSelectors
import Char
import ParseInt
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , delayedCommit
        , fail
        , float
        , ignore
        , keep
        , keyword
        , map
        , oneOf
        , oneOrMore
        , repeat
        , succeed
        , symbol
        , zeroOrMore
        )


parse : Parser CSSOM.CSSStyleSheet
parse =
    parseCSSStyleSheet


parseCSSStyleSheet : Parser CSSOM.CSSStyleSheet
parseCSSStyleSheet =
    repeat oneOrMore
        (succeed identity
            |. spaces
            |= parseRule
            |. spaces
        )


parseRule : Parser CSSOM.CSSRule
parseRule =
    succeed (\s d -> { selectors = s, declarations = d })
        |= parseSelectors
        |. spaces
        |= parseDeclarations


parseColor : Parser CSSBasicTypes.CSSColor
parseColor =
    oneOf
        [ parseColorKeyword
        , parseRGBAColor
        ]


parseRGBAColor : Parser CSSBasicTypes.CSSColor
parseRGBAColor =
    let
        parseHex =
            map (ParseInt.parseIntHex >> Result.withDefault 0)
                (keep (Parser.Exactly 2) Char.isHexDigit)
    in
    andThen
        (\maybeColor ->
            case maybeColor of
                Just color ->
                    succeed color

                Nothing ->
                    fail "bim"
        )
        (succeed (\r g b -> CSSBasicTypes.cssColorFromRGBA { red = r, green = g, blue = b, alpha = 1.0 })
            |. symbol "#"
            |= parseHex
            |= parseHex
            |= parseHex
        )


parseLength : Parser CSSBasicTypes.CSSLength
parseLength =
    andThen
        (\maybeLength ->
            case maybeLength of
                Just length ->
                    succeed length

                Nothing ->
                    fail "bim"
        )
        (succeed CSSBasicTypes.cssPixelLength
            |= float
            |. keyword "px"
        )


parseColorKeyword : Parser CSSBasicTypes.CSSColor
parseColorKeyword =
    andThen
        (\maybeColor ->
            case maybeColor of
                Just color ->
                    succeed color

                Nothing ->
                    fail "bim"
        )
        (succeed CSSBasicTypes.cssColorFromColorName
            |= parseIdentifier
        )


type IdOrClass
    = Id String
    | Class String


parseSimpleSelector : Parser CSSSelectors.CSSSelector
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
            map (always CSSSelectors.Universal) (symbol "*")

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
                    CSSSelectors.Simple
                        { tag = Just tag
                        , classes = classes
                        , ids = ids
                        }
                )
                |= tagSelector
                |= map separate classOrIdSelectors
            , succeed
                (\( ids, classes ) ->
                    CSSSelectors.Simple
                        { tag = Nothing
                        , classes = classes
                        , ids = ids
                        }
                )
                |= map separate classOrIdSelectors
            ]
        ]


parseSelectors : Parser (List CSSSelectors.CSSSelector)
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


parseDeclarationShorthands : Parser (List CSSOM.CSSDeclaration)
parseDeclarationShorthands =
    oneOf
        [ parseMarginShorthand
        ]


parseDeclaration : Parser CSSOM.CSSDeclaration
parseDeclaration =
    oneOf
        [ parseDisplay
        , parsePosition
        , parseMargin "margin-left" CSSOM.MarginLeft
        , parseMargin "margin-right" CSSOM.MarginRight
        , parseMargin "margin-top" CSSOM.MarginTop
        , parseMargin "margin-bottom" CSSOM.MarginBottom
        , parsePadding "padding-left" CSSOM.PaddingLeft
        , parsePadding "padding-right" CSSOM.PaddingRight
        , parsePadding "padding-top" CSSOM.PaddingTop
        , parsePadding "padding-bottom" CSSOM.PaddingBottom
        , parseBorderWidth "border-left-width" CSSOM.BorderLeftWidth
        , parseBorderWidth "border-right-width" CSSOM.BorderRightWidth
        , parseBorderWidth "border-top-width" CSSOM.BorderTopWidth
        , parseBorderWidth "border-bottom-width" CSSOM.BorderBottomWidth
        , parseHeight
        , parseWidth
        , parseBackgroundColor
        , parseBorderColor "border-top-color" CSSOM.BorderTopColor
        , parseBorderColor "border-bottom-color" CSSOM.BorderBottomColor
        , parseBorderColor "border-left-color" CSSOM.BorderLeftColor
        , parseBorderColor "border-right-color" CSSOM.BorderRightColor
        , parseBorderStyle "border-top-style" CSSOM.BorderTopStyle
        , parseBorderStyle "border-bottom-style" CSSOM.BorderBottomStyle
        , parseBorderStyle "border-left-style" CSSOM.BorderLeftStyle
        , parseBorderStyle "border-right-style" CSSOM.BorderRightStyle
        ]


parseBorderStyle :
    String
    -> (CSSOM.CSSBorderStyle CSSOM.SpecifiedValue -> CSSOM.CSSDeclaration)
    -> Parser CSSOM.CSSDeclaration
parseBorderStyle borderStyleName borderStyleConstructor =
    succeed borderStyleConstructor
        |. keyword borderStyleName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map (always CSSOM.borderStyleNone) (keyword "none")
            , map (always CSSOM.borderStyleSolid) (keyword "solid")
            ]
        |. spaces
        |. symbol ";"


parseBackgroundColor : Parser CSSOM.CSSDeclaration
parseBackgroundColor =
    succeed CSSOM.BackgroundColor
        |. keyword "background-color"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.backgroundColorColor parseColor
            , map (always CSSOM.backgroundColorTransparent) (keyword "transparent")
            ]
        |. spaces
        |. symbol ";"


parseBorderColor :
    String
    -> (CSSOM.CSSBorderColor CSSOM.SpecifiedValue -> CSSOM.CSSDeclaration)
    -> Parser CSSOM.CSSDeclaration
parseBorderColor borderName borderConstructor =
    succeed borderConstructor
        |. keyword borderName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.borderColorColor parseColor
            , map (always CSSOM.borderColorTransparent) (keyword "transparent")
            ]
        |. spaces
        |. symbol ";"


parseHeight : Parser CSSOM.CSSDeclaration
parseHeight =
    succeed CSSOM.Height
        |. keyword "height"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.heightLength parseLength
            , map (always CSSOM.heightAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parseWidth : Parser CSSOM.CSSDeclaration
parseWidth =
    succeed CSSOM.Width
        |. keyword "width"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.widthLength parseLength
            , map (always CSSOM.widthAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parseMarginShorthand : Parser (List CSSOM.CSSDeclaration)
parseMarginShorthand =
    andThen
        (\maybeMargins ->
            case maybeMargins of
                Just margins ->
                    succeed margins

                Nothing ->
                    fail "error in margin shorthand"
        )
        (succeed
            (\margins ->
                case margins of
                    [ all ] ->
                        Just [ CSSOM.MarginTop all, CSSOM.MarginRight all, CSSOM.MarginBottom all, CSSOM.MarginLeft all ]

                    [ vertical, horizontal ] ->
                        Just [ CSSOM.MarginTop vertical, CSSOM.MarginRight horizontal, CSSOM.MarginBottom vertical, CSSOM.MarginLeft horizontal ]

                    [ top, horizontal, bottom ] ->
                        Just [ CSSOM.MarginTop top, CSSOM.MarginRight horizontal, CSSOM.MarginBottom bottom, CSSOM.MarginLeft horizontal ]

                    [ top, right, bottom, left ] ->
                        Just [ CSSOM.MarginTop top, CSSOM.MarginRight right, CSSOM.MarginBottom bottom, CSSOM.MarginLeft left ]

                    _ ->
                        Nothing
            )
            |. keyword "margin"
            |. spaces
            |. symbol ":"
            |. spaces
            |= repeat oneOrMore
                (map CSSOM.marginLength
                    (succeed identity
                        |. spaces
                        |= parseLength
                    )
                )
            |. spaces
            |. symbol ";"
        )


parseMargin :
    String
    -> (CSSOM.CSSMargin CSSOM.SpecifiedValue -> CSSOM.CSSDeclaration)
    -> Parser CSSOM.CSSDeclaration
parseMargin marginName marginConstructor =
    succeed marginConstructor
        |. keyword marginName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.marginLength parseLength
            , map (always CSSOM.marginAuto) (keyword "auto")
            ]
        |. spaces
        |. symbol ";"


parsePadding :
    String
    -> (CSSOM.CSSPadding CSSOM.SpecifiedValue -> CSSOM.CSSDeclaration)
    -> Parser CSSOM.CSSDeclaration
parsePadding paddingName paddingConstructor =
    succeed paddingConstructor
        |. keyword paddingName
        |. spaces
        |. symbol ":"
        |. spaces
        |= map CSSOM.padding parseLength
        |. spaces
        |. symbol ";"


parseBorderWidth :
    String
    ->
        (CSSOM.CSSBorderWidth CSSOM.SpecifiedValue
         -> CSSOM.CSSDeclaration
        )
    -> Parser CSSOM.CSSDeclaration
parseBorderWidth borderWidthName borderWidthConstructor =
    succeed borderWidthConstructor
        |. keyword borderWidthName
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map CSSOM.borderWidthLength parseLength
            , map (always CSSOM.borderWidthThin) (keyword "thin")
            , map (always CSSOM.borderWidthMedium) (keyword "medium")
            , map (always CSSOM.borderWidthThick) (keyword "thick")
            ]
        |. spaces
        |. symbol ";"


parseDisplay : Parser CSSOM.CSSDeclaration
parseDisplay =
    succeed CSSOM.Display
        |. keyword "display"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map (always CSSOM.Block)
                (keyword "block")
            , map
                (always CSSOM.Inline)
                (keyword "inline")
            , map
                (always CSSOM.None)
                (keyword "none")
            ]
        |. spaces
        |. symbol ";"


parsePosition : Parser CSSOM.CSSDeclaration
parsePosition =
    succeed CSSOM.Position
        |. keyword "position"
        |. spaces
        |. symbol ":"
        |. spaces
        |= oneOf
            [ map (always CSSOM.Relative) (keyword "relative")
            , map (always CSSOM.Static) (keyword "static")
            ]
        |. spaces
        |. symbol ";"


parseDeclarations : Parser (List CSSOM.CSSDeclaration)
parseDeclarations =
    succeed List.concat
        |. symbol "{"
        |. spaces
        |= repeat zeroOrMore
            (succeed identity
                |. spaces
                |= oneOf [ map List.singleton parseDeclaration, parseDeclarationShorthands ]
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
