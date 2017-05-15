module Style exposing (..)

import Dict
import CSSOM exposing (..)
import DOM exposing (..)
import Color exposing (..)
import CSSBasicTypes exposing (..)
import CSSSelectors exposing (..)
import CSSOM exposing (..)


type alias StyledElementNode =
    { node : ElementNode
    , styles : Styles
    , children : List StyledNode
    }


type StyledRoot
    = StyledRoot StyledElementNode


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
    , borderLeftWidth = defaultBorderWidth
    , borderRightWidth = defaultBorderWidth
    , borderTopWidth = defaultBorderWidth
    , borderBottomWidth = defaultBorderWidth
    , width = defaultWidth
    , height = defaultHeight
    , backgroundColor = defaultBackgroundColor
    , borderTopColor = defaultBorderColor
    , borderBottomColor = defaultBorderColor
    , borderLeftColor = defaultBorderColor
    , borderRightColor = defaultBorderColor
    , borderLeftStyle = defaultBorderStyle
    , borderRightStyle = defaultBorderStyle
    , borderTopStyle = defaultBorderStyle
    , borderBottomStyle = defaultBorderStyle
    }


type alias Styles =
    { display : CSSDisplay
    , height : CSSHeight SpecifiedValue
    , width : CSSWidth SpecifiedValue
    , backgroundColor : CSSBackgroundColor SpecifiedValue
    , marginLeft : CSSMargin SpecifiedValue
    , marginRight : CSSMargin SpecifiedValue
    , marginTop : CSSMargin SpecifiedValue
    , marginBottom : CSSMargin SpecifiedValue
    , paddingLeft : CSSPadding SpecifiedValue
    , paddingRight : CSSPadding SpecifiedValue
    , paddingTop : CSSPadding SpecifiedValue
    , paddingBottom : CSSPadding SpecifiedValue
    , borderLeftWidth : CSSBorderWidth SpecifiedValue
    , borderRightWidth : CSSBorderWidth SpecifiedValue
    , borderTopWidth : CSSBorderWidth SpecifiedValue
    , borderBottomWidth : CSSBorderWidth SpecifiedValue
    , borderTopColor : CSSBorderColor SpecifiedValue
    , borderBottomColor : CSSBorderColor SpecifiedValue
    , borderLeftColor : CSSBorderColor SpecifiedValue
    , borderRightColor : CSSBorderColor SpecifiedValue
    , borderLeftStyle : CSSBorderStyle SpecifiedValue
    , borderRightStyle : CSSBorderStyle SpecifiedValue
    , borderTopStyle : CSSBorderStyle SpecifiedValue
    , borderBottomStyle : CSSBorderStyle SpecifiedValue
    }


styleTree : CSSStyleSheet -> DOMRoot -> StyledRoot
styleTree stylesheet (DOMRoot element) =
    StyledRoot
        { node = element
        , styles = specifiedValues element stylesheet
        , children = List.map (styleTreeChild stylesheet) (element.children)
        }


styleTreeChild : CSSStyleSheet -> DOMNode -> StyledNode
styleTreeChild stylesheet domNode =
    case domNode of
        Text text ->
            StyledText text

        Element elementNode ->
            let
                styles =
                    specifiedValues elementNode stylesheet

                children =
                    List.map (styleTreeChild stylesheet) elementNode.children
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

                    BorderLeftStyle value ->
                        { styles
                            | borderLeftStyle = value
                        }

                    BorderRightStyle value ->
                        { styles
                            | borderRightStyle = value
                        }

                    BorderTopStyle value ->
                        { styles
                            | borderTopStyle = value
                        }

                    BorderBottomStyle value ->
                        { styles
                            | borderBottomStyle = value
                        }
            )
            initialStyles
