module Style exposing (..)

import Dict
import CSSOM exposing (..)
import DOM exposing (..)
import Color exposing (..)


type CSSColor
    = CSSColor Color
    | Transparent


type CSSDimension
    = Auto
    | Length Float CSSUnit


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
    , height = Auto
    , marginLeft = Length 0 Pixel
    , marginRight = Length 0 Pixel
    , marginTop = Length 0 Pixel
    , marginBottom = Length 0 Pixel
    , paddingRight = Length 0 Pixel
    , paddingLeft = Length 0 Pixel
    , paddingTop = Length 0 Pixel
    , paddingBottom = Length 0 Pixel
    , borderLeft = Length 0 Pixel
    , borderRight = Length 0 Pixel
    , borderTop = Length 0 Pixel
    , borderBottom = Length 0 Pixel
    , width = Auto
    , backgroundColor = Transparent
    , borderTopColor = Transparent
    , borderBottomColor = Transparent
    , borderLeftColor = Transparent
    , borderRightColor = Transparent
    }


type alias Styles =
    { display : CSSDisplay
    , height : CSSDimension
    , backgroundColor : CSSColor
    , marginLeft : CSSDimension
    , marginRight : CSSDimension
    , marginTop : CSSDimension
    , marginBottom : CSSDimension
    , paddingLeft : CSSDimension
    , paddingRight : CSSDimension
    , paddingTop : CSSDimension
    , paddingBottom : CSSDimension
    , borderLeft : CSSDimension
    , borderRight : CSSDimension
    , borderTop : CSSDimension
    , borderBottom : CSSDimension
    , borderTopColor : CSSColor
    , borderBottomColor : CSSColor
    , borderLeftColor : CSSColor
    , borderRightColor : CSSColor
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


color : CSSValue -> Maybe CSSColor
color value =
    case value of
        ColorValue (RGBA color) ->
            Just (CSSColor color)

        _ ->
            Nothing


margin : CSSValue -> Maybe CSSDimension
margin value =
    case value of
        Keyword CSSOM.Auto ->
            Just Auto

        CSSOM.Length l u ->
            Just <| Length l u

        _ ->
            Nothing


padding : CSSValue -> Maybe CSSDimension
padding value =
    case value of
        CSSOM.Length l u ->
            Just <| Length l u

        _ ->
            Nothing


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
