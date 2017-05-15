module Main exposing (..)

import Html exposing (..)
import Color exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import HtmlParser
import AnonymousBox
import Parser
import DOM
import CSSParser
import Style
import Layout
import Painting
import BoxModel


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { paintingCommands : List Painting.DisplayCommand
    , html : String
    , css : String
    }


type Msg
    = CSS String
    | HTML String


startCSS =
    """
.bang {
    height: 200px;
    display: block;
    background-color: #333333;
}
.bam {
    width: auto;
    height: 100px;
    display:block;
    background-color: #ff0000;
    padding-top: 10px;
    border-top-width: 10px;
    border-top-color: white;
    border-bottom-width: 10px;
    border-bottom-color: white;
    border-left-width: 10px;
    border-left-color: white;
    border-right-width: 10px;
    border-right-color: white;
    border-top-style: solid;
    border-bottom-style: solid;
    border-left-style: solid;
    border-right-style: solid;
    margin-top: 10px;
}
#boum{
    width: 50px;
    height: 50px;
    display: block;
    background-color: #00ff00;
}
"""


startHTML =
    """
<div class="bang">
    <div id="boum"></div>
    <div class="bam"></div>
</div>
"""


init : ( Model, Cmd Msg )
init =
    ( { paintingCommands = render startHTML startCSS
      , css = startCSS
      , html = startHTML
      }
    , Cmd.none
    )


render : String -> String -> List Painting.DisplayCommand
render html css =
    let
        dom =
            Result.mapError (always "html parse error") (Parser.run HtmlParser.parse html)

        cssom =
            Result.mapError (always "css parse error") (Parser.run CSSParser.parse css)

        style =
            Result.map2 Style.styleTree cssom dom

        containingBlock =
            BoxModel.boxModel
                { x = 0, y = 0, width = 400, height = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }

        boxes =
            case Result.map AnonymousBox.boxTree style of
                Ok (Just box) ->
                    Ok box

                Ok Nothing ->
                    Err "no box"

                Err err ->
                    Err err

        layout =
            Result.map2 Layout.startLayout
                boxes
                (Ok containingBlock)

        displayCommand =
            Result.map Painting.buildDisplayList layout
    in
        case displayCommand of
            Ok dc ->
                dc

            _ ->
                []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { paintingCommands, html, css } =
    case msg of
        CSS newCSS ->
            ( { paintingCommands = render html newCSS, html = html, css = newCSS }, Cmd.none )

        HTML newHTML ->
            ( { paintingCommands = render newHTML css, html = newHTML, css = css }, Cmd.none )


view : Model -> Html Msg
view { paintingCommands, html, css } =
    div []
        [ div [] (List.map element paintingCommands)
        , ui html css
        ]


ui : String -> String -> Html Msg
ui html css =
    let
        textareaStyle =
            [ ( "display", "block" ), ( "width", "400px" ), ( "height", "400px" ) ]
    in
        div
            [ style [ ( "float", "right" ) ] ]
            [ textarea [ style textareaStyle, onInput HTML ] [ text html ]
            , textarea [ style textareaStyle, onInput CSS ] [ text css ]
            ]


element : Painting.DisplayCommand -> Html Msg
element (Painting.SolidColor { x, y, width, height } color) =
    let
        toRGBAString { red, green, blue, alpha } =
            "rgba("
                ++ toString red
                ++ ", "
                ++ toString green
                ++ ", "
                ++ toString blue
                ++ ", "
                ++ toString alpha
                ++ ")"

        s =
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "background", toRGBAString color )
            ]
    in
        div [ style s ] []


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
