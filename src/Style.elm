module Style exposing (..)

import Dict
import CSSOM exposing (..)
import DOM exposing (..)
import Color exposing (..)
import CSSBasicTypes exposing (..)


type alias MatchedRule =
    { specifity : Int
    , rule : CSSRule
    }


type alias StyledElementNode =
    { node : ElementNode
    , styles : Styles
    , children : List StyledNode
    }


type StyledNode
    = StyledElement StyledElementNode
    | StyledText String


initialStyles : Styles
initialStyles =
    { display = Inline
    , marginLeft = defaultMargin
    , marginRight = defaultMargin
    , marginTop = defaultMargin
    , marginBottom = defaultMargin
    , paddingLeft = defaultPadding
    , paddingRight = defaultPadding
    , paddingTop = defaultPadding
    , paddingBottom = defaultPadding
    , borderLeftWidth = BorderWidthMedium
    , borderRightWidth = BorderWidthMedium
    , borderTopWidth = BorderWidthMedium
    , borderBottomWidth = BorderWidthMedium
    , width = WidthAuto
    , height = HeightAuto
    , backgroundColor = BackgroundColorTransparent
    , borderTopColor = BorderColorTransparent
    , borderBottomColor = BorderColorTransparent
    , borderLeftColor = BorderColorTransparent
    , borderRightColor = BorderColorTransparent
    }


type alias Styles =
    { display : CSSDisplay
    , height : CSSHeight
    , width : CSSWidth
    , backgroundColor : CSSBackgroundColor
    , marginLeft : CSSMargin SpecifiedValue
    , marginRight : CSSMargin SpecifiedValue
    , marginTop : CSSMargin SpecifiedValue
    , marginBottom : CSSMargin SpecifiedValue
    , paddingLeft : CSSPadding SpecifiedValue
    , paddingRight : CSSPadding SpecifiedValue
    , paddingTop : CSSPadding SpecifiedValue
    , paddingBottom : CSSPadding SpecifiedValue
    , borderLeftWidth : CSSBorderWidth
    , borderRightWidth : CSSBorderWidth
    , borderTopWidth : CSSBorderWidth
    , borderBottomWidth : CSSBorderWidth
    , borderTopColor : CSSBorderColor
    , borderBottomColor : CSSBorderColor
    , borderLeftColor : CSSBorderColor
    , borderRightColor : CSSBorderColor
    }


styleTree : CSSStyleSheet -> DOMNode -> StyledNode
styleTree stylesheet domNode =
    case domNode of
        Text text ->
            StyledText text

        Element elementNode ->
            let
                styles =
                    specifiedValues elementNode stylesheet

                children =
                    List.map (styleTree stylesheet) elementNode.children
            in
                StyledElement
                    { node = elementNode
                    , styles = styles
                    , children = children
                    }


specifiedValues : ElementNode -> CSSStyleSheet -> Styles
specifiedValues node stylesheet =
    stylesheet
        |> matchingRules node
        |> List.sortBy .specifity
        |> List.map .rule
        |> List.map .declarations
        |> List.concat
        |> List.foldl
            (\declaration styles ->
                case declaration of
                    Display value ->
                        { styles
                            | display = value
                        }

                    MarginLeft value ->
                        { styles
                            | marginLeft = value
                        }

                    MarginRight value ->
                        { styles
                            | marginRight = value
                        }

                    MarginTop value ->
                        { styles
                            | marginTop = value
                        }

                    MarginBottom value ->
                        { styles
                            | marginBottom = value
                        }

                    PaddingLeft value ->
                        { styles
                            | paddingLeft = value
                        }

                    PaddingRight value ->
                        { styles
                            | paddingRight = value
                        }

                    PaddingTop value ->
                        { styles
                            | paddingTop = value
                        }

                    PaddingBottom value ->
                        { styles
                            | paddingBottom = value
                        }

                    Height value ->
                        { styles
                            | height = value
                        }

                    Width value ->
                        { styles
                            | width = value
                        }

                    BackgroundColor value ->
                        { styles
                            | backgroundColor = value
                        }

                    BorderLeftWidth value ->
                        { styles
                            | borderLeftWidth = value
                        }

                    BorderRightWidth value ->
                        { styles
                            | borderRightWidth = value
                        }

                    BorderTopWidth value ->
                        { styles
                            | borderTopWidth = value
                        }

                    BorderBottomWidth value ->
                        { styles
                            | borderBottomWidth = value
                        }

                    BorderTopColor value ->
                        { styles
                            | borderTopColor = value
                        }

                    BorderBottomColor value ->
                        { styles
                            | borderBottomColor = value
                        }

                    BorderLeftColor value ->
                        { styles
                            | borderLeftColor = value
                        }

                    BorderRightColor value ->
                        { styles
                            | borderRightColor = value
                        }
            )
            initialStyles


matchingRules : ElementNode -> CSSStyleSheet -> List MatchedRule
matchingRules node stylesheet =
    List.filterMap (matchRule node) stylesheet


matchRule : ElementNode -> CSSRule -> Maybe MatchedRule
matchRule node rule =
    rule.selectors
        |> List.filter (matches node)
        |> List.head
        |> Maybe.map
            (\selector ->
                MatchedRule
                    (specifity selector)
                    rule
            )


matches : ElementNode -> CSSSelector -> Bool
matches node selector =
    case selector of
        Universal ->
            True

        Simple s ->
            matchesSimpleSelector node s


matchesSimpleSelector : ElementNode -> CSSSimpleSelector -> Bool
matchesSimpleSelector { tagName, attributes } { tag, ids, classes } =
    let
        id =
            Dict.get "id" attributes

        className =
            Dict.get "class" attributes

        matchesTagSelector =
            Maybe.withDefault True <|
                Maybe.map ((==) tagName) tag

        matchesIdSelector =
            Maybe.withDefault True <|
                Maybe.map (\id -> List.member id ids) id

        matchesClassSelector =
            Maybe.withDefault True <|
                Maybe.map (\class -> List.member class classes) className
    in
        matchesTagSelector
            && matchesIdSelector
            && matchesClassSelector
