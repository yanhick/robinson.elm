module Style
    exposing
        ( StyledNode
            ( StyledElement
            , StyledText
            )
        , StyledRoot(StyledRoot)
        , Styles
        , initialStyles
        , styleTree
        )

import CSSOM
import DOM


type alias StyledElementNode =
    { node : DOM.ElementNode
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
    { display = CSSOM.Inline
    , marginLeft = CSSOM.defaultMargin
    , marginRight = CSSOM.defaultMargin
    , marginTop = CSSOM.defaultMargin
    , marginBottom = CSSOM.defaultMargin
    , paddingLeft = CSSOM.defaultPadding
    , paddingRight = CSSOM.defaultPadding
    , paddingTop = CSSOM.defaultPadding
    , paddingBottom = CSSOM.defaultPadding
    , borderLeftWidth = CSSOM.defaultBorderWidth
    , borderRightWidth = CSSOM.defaultBorderWidth
    , borderTopWidth = CSSOM.defaultBorderWidth
    , borderBottomWidth = CSSOM.defaultBorderWidth
    , width = CSSOM.defaultWidth
    , height = CSSOM.defaultHeight
    , backgroundColor = CSSOM.defaultBackgroundColor
    , borderTopColor = CSSOM.defaultBorderColor
    , borderBottomColor = CSSOM.defaultBorderColor
    , borderLeftColor = CSSOM.defaultBorderColor
    , borderRightColor = CSSOM.defaultBorderColor
    , borderLeftStyle = CSSOM.defaultBorderStyle
    , borderRightStyle = CSSOM.defaultBorderStyle
    , borderTopStyle = CSSOM.defaultBorderStyle
    , borderBottomStyle = CSSOM.defaultBorderStyle
    }


type alias Styles =
    { display : CSSOM.CSSDisplay
    , height : CSSOM.CSSHeight CSSOM.SpecifiedValue
    , width : CSSOM.CSSWidth CSSOM.SpecifiedValue
    , backgroundColor : CSSOM.CSSBackgroundColor CSSOM.SpecifiedValue
    , marginLeft : CSSOM.CSSMargin CSSOM.SpecifiedValue
    , marginRight : CSSOM.CSSMargin CSSOM.SpecifiedValue
    , marginTop : CSSOM.CSSMargin CSSOM.SpecifiedValue
    , marginBottom : CSSOM.CSSMargin CSSOM.SpecifiedValue
    , paddingLeft : CSSOM.CSSPadding CSSOM.SpecifiedValue
    , paddingRight : CSSOM.CSSPadding CSSOM.SpecifiedValue
    , paddingTop : CSSOM.CSSPadding CSSOM.SpecifiedValue
    , paddingBottom : CSSOM.CSSPadding CSSOM.SpecifiedValue
    , borderLeftWidth : CSSOM.CSSBorderWidth CSSOM.SpecifiedValue
    , borderRightWidth : CSSOM.CSSBorderWidth CSSOM.SpecifiedValue
    , borderTopWidth : CSSOM.CSSBorderWidth CSSOM.SpecifiedValue
    , borderBottomWidth : CSSOM.CSSBorderWidth CSSOM.SpecifiedValue
    , borderTopColor : CSSOM.CSSBorderColor CSSOM.SpecifiedValue
    , borderBottomColor : CSSOM.CSSBorderColor CSSOM.SpecifiedValue
    , borderLeftColor : CSSOM.CSSBorderColor CSSOM.SpecifiedValue
    , borderRightColor : CSSOM.CSSBorderColor CSSOM.SpecifiedValue
    , borderLeftStyle : CSSOM.CSSBorderStyle CSSOM.SpecifiedValue
    , borderRightStyle : CSSOM.CSSBorderStyle CSSOM.SpecifiedValue
    , borderTopStyle : CSSOM.CSSBorderStyle CSSOM.SpecifiedValue
    , borderBottomStyle : CSSOM.CSSBorderStyle CSSOM.SpecifiedValue
    }


styleTree : CSSOM.CSSStyleSheet -> DOM.DOMRoot -> StyledRoot
styleTree stylesheet (DOM.DOMRoot element) =
    StyledRoot
        { node = element
        , styles = specifiedValues element stylesheet
        , children = List.map (styleTreeChild stylesheet) element.children
        }


styleTreeChild : CSSOM.CSSStyleSheet -> DOM.DOMNode -> StyledNode
styleTreeChild stylesheet domNode =
    case domNode of
        DOM.Text text ->
            StyledText text

        DOM.Element elementNode ->
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


specifiedValues : DOM.ElementNode -> CSSOM.CSSStyleSheet -> Styles
specifiedValues node stylesheet =
    stylesheet
        |> CSSOM.matchingRules node
        |> List.sortBy .specifity
        |> List.map .rule
        |> List.map .declarations
        |> List.concat
        |> List.foldl
            (\declaration styles ->
                case declaration of
                    CSSOM.Display value ->
                        { styles
                            | display = value
                        }

                    CSSOM.MarginLeft value ->
                        { styles
                            | marginLeft = value
                        }

                    CSSOM.MarginRight value ->
                        { styles
                            | marginRight = value
                        }

                    CSSOM.MarginTop value ->
                        { styles
                            | marginTop = value
                        }

                    CSSOM.MarginBottom value ->
                        { styles
                            | marginBottom = value
                        }

                    CSSOM.PaddingLeft value ->
                        { styles
                            | paddingLeft = value
                        }

                    CSSOM.PaddingRight value ->
                        { styles
                            | paddingRight = value
                        }

                    CSSOM.PaddingTop value ->
                        { styles
                            | paddingTop = value
                        }

                    CSSOM.PaddingBottom value ->
                        { styles
                            | paddingBottom = value
                        }

                    CSSOM.Height value ->
                        { styles
                            | height = value
                        }

                    CSSOM.Width value ->
                        { styles
                            | width = value
                        }

                    CSSOM.BackgroundColor value ->
                        { styles
                            | backgroundColor = value
                        }

                    CSSOM.BorderLeftWidth value ->
                        { styles
                            | borderLeftWidth = value
                        }

                    CSSOM.BorderRightWidth value ->
                        { styles
                            | borderRightWidth = value
                        }

                    CSSOM.BorderTopWidth value ->
                        { styles
                            | borderTopWidth = value
                        }

                    CSSOM.BorderBottomWidth value ->
                        { styles
                            | borderBottomWidth = value
                        }

                    CSSOM.BorderTopColor value ->
                        { styles
                            | borderTopColor = value
                        }

                    CSSOM.BorderBottomColor value ->
                        { styles
                            | borderBottomColor = value
                        }

                    CSSOM.BorderLeftColor value ->
                        { styles
                            | borderLeftColor = value
                        }

                    CSSOM.BorderRightColor value ->
                        { styles
                            | borderRightColor = value
                        }

                    CSSOM.BorderLeftStyle value ->
                        { styles
                            | borderLeftStyle = value
                        }

                    CSSOM.BorderRightStyle value ->
                        { styles
                            | borderRightStyle = value
                        }

                    CSSOM.BorderTopStyle value ->
                        { styles
                            | borderTopStyle = value
                        }

                    CSSOM.BorderBottomStyle value ->
                        { styles
                            | borderBottomStyle = value
                        }
            )
            initialStyles
