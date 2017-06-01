module CSSSelectors
    exposing
        ( CSSSelector(Simple, Universal)
        , matches
        , specifity
        )

import DOM
import Dict


type alias CSSSimpleSelector =
    { tag : Maybe String
    , classes : List String
    , ids : List String
    }


type CSSSelector
    = Simple CSSSimpleSelector
    | Universal


specifity : CSSSelector -> Int
specifity selector =
    case selector of
        Simple { tag, classes, ids } ->
            List.length classes
                + List.length ids
                + Maybe.withDefault 0
                    (Maybe.map (always 1) tag)

        Universal ->
            0


matches : DOM.ElementNode -> CSSSelector -> Bool
matches node selector =
    case selector of
        Universal ->
            True

        Simple s ->
            matchesSimpleSelector node s


matchesSimpleSelector : DOM.ElementNode -> CSSSimpleSelector -> Bool
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
