module Main exposing (..)

import Box
import BoxModel
import CSSParser
import Color exposing (..)
import DOM
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import HtmlParser
import Layout
import Painting
import Parser
import Style


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
    height: 500px;
    display: block;
    background-color: #333333;
}
.bam {
    width: auto;
    display:block;
    background-color: #ff0000;
    padding-bottom: 10px;
    padding-top: 10px;
    padding-left: 10px;
    padding-right: 10px;
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
    position: relative;
    top: 20px;
    left: 30px;
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
    <div class="bam">
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis varius mi et lacus hendrerit, at fermentum nunc imperdiet. Nullam pulvinar et nibh at auctor.
    ADD MORE TEXT HERE
    </div>
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
            BoxModel.make
                { x = 0, y = 0, width = 400, height = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }
                { top = 0, left = 0, bottom = 0, right = 0 }

        boxes =
            Result.map Box.boxTree style

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
element command =
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
    in
    case command of
        Painting.SolidColor { x, y, width, height } color ->
            div
                [ style
                    [ ( "width", toString width ++ "px" )
                    , ( "height", toString height ++ "px" )
                    , ( "position", "absolute" )
                    , ( "left", toString x ++ "px" )
                    , ( "top", toString y ++ "px" )
                    , ( "background", toRGBAString color )
                    ]
                ]
                []

        Painting.Text { x, y, width, height } textContent ->
            div
                [ style
                    [ ( "width", toString width ++ "px" )
                    , ( "height", toString height ++ "px" )
                    , ( "position", "absolute" )
                    , ( "left", toString x ++ "px" )
                    , ( "top", toString y ++ "px" )
                    ]
                ]
                [ text textContent ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
