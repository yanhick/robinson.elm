module Style exposing (..)

import Dict
import CSSOM exposing (..)
import DOM exposing (..)


type CSSDisplay
    = Inline
    | Block
    | None


type CSSDimension
    = Auto
    | Length Float CSSUnit


type alias MatchedRule =
    { specifity : Int
    , rule : CSSRule
    }


type StyledNode
    = StyledElement
        { node : ElementNode
        , styles : Styles
        , children : List StyledNode
        }
    | StyledText String


type alias Styles =
    { display : CSSDisplay
    , height : CSSDimension
    , marginLeft : CSSDimension
    , marginRight : CSSDimension
    , paddingLeft : CSSDimension
    , paddingRight : CSSDimension
    , borderLeft : CSSDimension
    , borderRight : CSSDimension
    , width : CSSDimension
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


display : CSSValue -> CSSDisplay
display value =
    case value of
        Keyword word ->
            case word of
                "inline" ->
                    Inline

                "block" ->
                    Block

                _ ->
                    None

        _ ->
            None


specifiedValues : ElementNode -> CSSStyleSheet -> Styles
specifiedValues node stylesheet =
    stylesheet
        |> matchingRules node
        |> List.sortBy .specifity
        |> List.map .rule
        |> List.map .declarations
        |> List.concat
        |> List.foldl
            (\{ name, value } styles ->
                case name of
                    "display" ->
                        { styles | display = display value }

                    _ ->
                        styles
            )
            { display = None
            , height = Auto
            , marginLeft = Length 0.0 Pixel
            , marginRight = Length 0.0 Pixel
            , paddingLeft = Length 0.0 Pixel
            , paddingRight = Length 0.0 Pixel
            , borderLeft = Length 0.0 Pixel
            , borderRight = Length 0.0 Pixel
            , width = Auto
            }


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
