module CSSOM
    exposing
        ( CSSBackgroundColor
        , CSSBorderColor
        , CSSBorderStyle
        , CSSBorderWidth
        , CSSDeclaration(..)
        , CSSDisplay(..)
        , CSSHeight
        , CSSMargin
        , CSSOffset
        , CSSPadding
        , CSSPosition(..)
        , CSSRule
        , CSSStyleSheet
        , CSSWidth
        , SpecifiedValue
        , backgroundColorColor
        , backgroundColorTransparent
        , borderColorColor
        , borderColorTransparent
        , borderStyleNone
        , borderStyleSolid
        , borderWidthLength
        , borderWidthMedium
        , borderWidthThick
        , borderWidthThin
        , computedBorderWidth
        , computedHeight
        , computedMargin
        , computedPadding
        , computedWidth
        , defaultBackgroundColor
        , defaultBorderColor
        , defaultBorderStyle
        , defaultBorderWidth
        , defaultHeight
        , defaultMargin
        , defaultOffset
        , defaultPadding
        , defaultWidth
        , heightAuto
        , heightLength
        , isAutoMargin
        , isAutoWidth
        , marginAuto
        , marginLength
        , matchingRules
        , offsetAuto
        , offsetLength
        , padding
        , usedBackgroundColor
        , usedBorderColor
        , usedBorderWidth
        , usedHeight
        , usedMargin
        , usedPadding
        , usedWidth
        , widthAuto
        , widthLength
        )

import CSSBasicTypes
import CSSSelectors
import DOM


type SpecifiedValue
    = SpecifiedValue


type ComputedValue
    = ComputedValue


type CSSDisplay
    = Block
    | Inline
    | None


type CSSPosition
    = Static
    | Relative


type CSSMargin valueType
    = MarginAuto
    | MarginLength CSSBasicTypes.CSSLength


type CSSOffset valueType
    = OffsetAuto
    | OffsetLength CSSBasicTypes.CSSLength


defaultOffset : CSSOffset SpecifiedValue
defaultOffset =
    OffsetAuto


offsetLength : CSSBasicTypes.CSSLength -> CSSOffset SpecifiedValue
offsetLength =
    OffsetLength


marginLength : CSSBasicTypes.CSSLength -> CSSMargin SpecifiedValue
marginLength =
    MarginLength


offsetAuto : CSSOffset SpecifiedValue
offsetAuto =
    OffsetAuto


marginAuto : CSSMargin SpecifiedValue
marginAuto =
    MarginAuto


defaultMargin : CSSMargin SpecifiedValue
defaultMargin =
    MarginLength CSSBasicTypes.defaultCSSLength


isAutoMargin : CSSMargin valueType -> Bool
isAutoMargin margin =
    case margin of
        MarginAuto ->
            True

        _ ->
            False


computedMargin : CSSMargin SpecifiedValue -> CSSMargin ComputedValue
computedMargin margin =
    case margin of
        MarginLength length ->
            MarginLength length

        MarginAuto ->
            MarginAuto


usedMargin : CSSMargin ComputedValue -> Maybe Float
usedMargin margin =
    case margin of
        MarginLength length ->
            Just <| CSSBasicTypes.computedCSSLength length

        MarginAuto ->
            Nothing


type CSSPadding valueType
    = PaddingLength CSSBasicTypes.CSSLength


defaultPadding : CSSPadding SpecifiedValue
defaultPadding =
    PaddingLength CSSBasicTypes.defaultCSSLength


padding : CSSBasicTypes.CSSLength -> CSSPadding SpecifiedValue
padding =
    PaddingLength


computedPadding : CSSPadding SpecifiedValue -> CSSPadding ComputedValue
computedPadding (PaddingLength length) =
    PaddingLength length


usedPadding : CSSPadding ComputedValue -> Float
usedPadding (PaddingLength length) =
    CSSBasicTypes.computedCSSLength length


type CSSHeight valueType
    = HeightAuto
    | HeightLength CSSBasicTypes.CSSLength


heightLength : CSSBasicTypes.CSSLength -> CSSHeight SpecifiedValue
heightLength =
    HeightLength


heightAuto : CSSHeight SpecifiedValue
heightAuto =
    HeightAuto


defaultHeight : CSSHeight SpecifiedValue
defaultHeight =
    HeightAuto


computedHeight : CSSHeight SpecifiedValue -> CSSHeight ComputedValue
computedHeight height =
    case height of
        HeightAuto ->
            HeightAuto

        HeightLength length ->
            HeightLength length


usedHeight : CSSHeight ComputedValue -> Maybe Float
usedHeight height =
    case height of
        HeightAuto ->
            Nothing

        HeightLength length ->
            Just <| CSSBasicTypes.computedCSSLength length


type CSSWidth valueType
    = WidthAuto
    | WidthLength CSSBasicTypes.CSSLength


widthLength : CSSBasicTypes.CSSLength -> CSSWidth SpecifiedValue
widthLength =
    WidthLength


widthAuto : CSSWidth SpecifiedValue
widthAuto =
    WidthAuto


isAutoWidth : CSSWidth valueType -> Bool
isAutoWidth width =
    case width of
        WidthAuto ->
            True

        _ ->
            False


computedWidth : CSSWidth SpecifiedValue -> CSSWidth ComputedValue
computedWidth width =
    case width of
        WidthAuto ->
            WidthAuto

        WidthLength length ->
            WidthLength length


defaultWidth : CSSWidth SpecifiedValue
defaultWidth =
    WidthAuto


usedWidth : CSSWidth ComputedValue -> Maybe Float
usedWidth width =
    case width of
        WidthAuto ->
            Nothing

        WidthLength length ->
            Just <| CSSBasicTypes.computedCSSLength length


type CSSBackgroundColor valueType
    = BackgroundColorColor CSSBasicTypes.CSSColor
    | BackgroundColorTransparent


backgroundColorColor : CSSBasicTypes.CSSColor -> CSSBackgroundColor SpecifiedValue
backgroundColorColor =
    BackgroundColorColor


backgroundColorTransparent : CSSBackgroundColor SpecifiedValue
backgroundColorTransparent =
    BackgroundColorTransparent


defaultBackgroundColor : CSSBackgroundColor SpecifiedValue
defaultBackgroundColor =
    BackgroundColorTransparent


usedBackgroundColor : CSSBackgroundColor SpecifiedValue -> CSSBasicTypes.RGBAColor
usedBackgroundColor backgroundColor =
    case backgroundColor of
        BackgroundColorColor color ->
            CSSBasicTypes.computedCSSColor color

        BackgroundColorTransparent ->
            { red = 0
            , green = 0
            , blue = 0
            , alpha = 0
            }


type CSSBorderStyle valueType
    = BorderStyleNone
    | BorderStyleSolid


borderStyleNone : CSSBorderStyle SpecifiedValue
borderStyleNone =
    BorderStyleNone


borderStyleSolid : CSSBorderStyle SpecifiedValue
borderStyleSolid =
    BorderStyleSolid


defaultBorderStyle : CSSBorderStyle SpecifiedValue
defaultBorderStyle =
    BorderStyleNone


type CSSBorderColor valueType
    = BorderColorColor CSSBasicTypes.CSSColor
    | BorderColorTransparent


borderColorColor : CSSBasicTypes.CSSColor -> CSSBorderColor SpecifiedValue
borderColorColor =
    BorderColorColor


borderColorTransparent : CSSBorderColor SpecifiedValue
borderColorTransparent =
    BorderColorTransparent


defaultBorderColor : CSSBorderColor SpecifiedValue
defaultBorderColor =
    BorderColorTransparent


usedBorderColor : CSSBorderColor SpecifiedValue -> CSSBasicTypes.RGBAColor
usedBorderColor borderColor =
    case borderColor of
        BorderColorColor color ->
            CSSBasicTypes.computedCSSColor color

        BorderColorTransparent ->
            { red = 0
            , green = 0
            , blue = 0
            , alpha = 0
            }


type CSSBorderWidth valueType
    = BorderWidthThin
    | BorderWidthMedium
    | BorderWidthThick
    | BorderWidthLength CSSBasicTypes.CSSLength


defaultBorderWidth : CSSBorderWidth SpecifiedValue
defaultBorderWidth =
    BorderWidthMedium


computedBorderWidth :
    CSSBorderStyle SpecifiedValue
    -> CSSBorderWidth SpecifiedValue
    -> CSSBorderWidth ComputedValue
computedBorderWidth borderStyle borderWidth =
    case borderStyle of
        BorderStyleNone ->
            BorderWidthLength CSSBasicTypes.defaultCSSLength

        BorderStyleSolid ->
            case borderWidth of
                BorderWidthThin ->
                    BorderWidthThin

                BorderWidthMedium ->
                    BorderWidthMedium

                BorderWidthThick ->
                    BorderWidthThick

                BorderWidthLength length ->
                    BorderWidthLength length


usedBorderWidth : CSSBorderWidth ComputedValue -> Float
usedBorderWidth borderWidth =
    case borderWidth of
        BorderWidthThin ->
            1

        BorderWidthMedium ->
            5

        BorderWidthThick ->
            10

        BorderWidthLength length ->
            CSSBasicTypes.computedCSSLength length


borderWidthThick : CSSBorderWidth SpecifiedValue
borderWidthThick =
    BorderWidthThick


borderWidthMedium : CSSBorderWidth SpecifiedValue
borderWidthMedium =
    BorderWidthMedium


borderWidthThin : CSSBorderWidth SpecifiedValue
borderWidthThin =
    BorderWidthThin


borderWidthLength : CSSBasicTypes.CSSLength -> CSSBorderWidth SpecifiedValue
borderWidthLength =
    BorderWidthLength


type CSSDeclaration
    = Display CSSDisplay
    | Position CSSPosition
    | Top (CSSOffset SpecifiedValue)
    | Bottom (CSSOffset SpecifiedValue)
    | Left (CSSOffset SpecifiedValue)
    | Right (CSSOffset SpecifiedValue)
    | MarginLeft (CSSMargin SpecifiedValue)
    | MarginRight (CSSMargin SpecifiedValue)
    | MarginTop (CSSMargin SpecifiedValue)
    | MarginBottom (CSSMargin SpecifiedValue)
    | PaddingLeft (CSSPadding SpecifiedValue)
    | PaddingRight (CSSPadding SpecifiedValue)
    | PaddingTop (CSSPadding SpecifiedValue)
    | PaddingBottom (CSSPadding SpecifiedValue)
    | Height (CSSHeight SpecifiedValue)
    | Width (CSSWidth SpecifiedValue)
    | BackgroundColor (CSSBackgroundColor SpecifiedValue)
    | BorderLeftWidth (CSSBorderWidth SpecifiedValue)
    | BorderRightWidth (CSSBorderWidth SpecifiedValue)
    | BorderTopWidth (CSSBorderWidth SpecifiedValue)
    | BorderBottomWidth (CSSBorderWidth SpecifiedValue)
    | BorderTopColor (CSSBorderColor SpecifiedValue)
    | BorderBottomColor (CSSBorderColor SpecifiedValue)
    | BorderLeftColor (CSSBorderColor SpecifiedValue)
    | BorderRightColor (CSSBorderColor SpecifiedValue)
    | BorderLeftStyle (CSSBorderStyle SpecifiedValue)
    | BorderRightStyle (CSSBorderStyle SpecifiedValue)
    | BorderTopStyle (CSSBorderStyle SpecifiedValue)
    | BorderBottomStyle (CSSBorderStyle SpecifiedValue)


type alias CSSRule =
    { selectors : List CSSSelectors.CSSSelector
    , declarations : List CSSDeclaration
    }


type alias CSSStyleSheet =
    List CSSRule


matchingRules : DOM.ElementNode -> CSSStyleSheet -> List MatchedRule
matchingRules node stylesheet =
    List.filterMap (matchRule node) stylesheet


matchRule : DOM.ElementNode -> CSSRule -> Maybe MatchedRule
matchRule node rule =
    rule.selectors
        |> List.filter (CSSSelectors.matches node)
        |> List.head
        |> Maybe.map
            (\selector ->
                MatchedRule
                    (CSSSelectors.specifity selector)
                    rule
            )


type alias MatchedRule =
    { specifity : Int
    , rule : CSSRule
    }
