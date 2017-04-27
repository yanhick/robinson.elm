module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import HtmlParser
import Parser
import DOM
import CSSParser
import Style
import Layout
import Painting


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    List Painting.DisplayCommand


type Msg
    = Start


init : ( Model, Cmd Msg )
init =
    ( []
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update Start _ =
    let
        html =
            Debug.log "html" <|
                Parser.run HtmlParser.parse "<div class=\"bang\"><div id=\"boum\"></div><div class=\"bam\"></div></div>"

        css =
            Parser.run CSSParser.parse ".bang { width: 200px; height: 200px; display: block; }.bam { width: 100px; height: 100px; display:block; } #boum{ width: 50px; height: 50px; display: block; }"

        style =
            Result.map2 Style.styleTree css html

        layout =
            Result.map2 Layout.startLayout style (Ok Layout.initialDimensions)

        l =
            case layout of
                Ok (Layout.LayoutBox { children }) ->
                    Debug.log "children" <|
                        List.map (\(Layout.LayoutBox { dimensions }) -> dimensions) children

                Err _ ->
                    Debug.crash "oups"

        displayCommand =
            Result.map Painting.buildDisplayList layout
    in
        case displayCommand of
            Ok dc ->
                ( dc, Cmd.none )

            Err _ ->
                ( [], Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        [] ->
            div [ onClick Start ] [ text "start" ]

        _ ->
            div [] (List.map element model)


element : Painting.DisplayCommand -> Html Msg
element (Painting.SolidColor { x, y, width, height }) =
    let
        s =
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "position", "absolute" )
            , ( "left", toString x ++ "px" )
            , ( "top", toString y ++ "px" )
            , ( "background", "black" )
            ]
    in
        div [ style s ] []


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
