port module Main exposing (Message(..), Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, audio, button, div, h1, img, p, source, span, text)
import Html.Attributes exposing (autoplay, src)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Time


port audioplay : Encode.Value -> Cmd msg



---- MODEL ----


type alias Model =
    { now : Time.Posix
    , startTid : Time.Posix
    , läge : Läge
    }


init : ( Model, Cmd Message )
init =
    ( { now = Time.millisToPosix 0
      , startTid = Time.millisToPosix 0
      , läge = InnanStart
      }
    , Cmd.none
    )



---- UPDATE ----


type Message
    = Beat Time.Posix
    | StartClick
    | StopClick


type Läge
    = InnanStart
    | Träning
    | Vila


träningstid =
    60 * 1000


vilotid =
    10 * 1000


startLabel =
    "start"


bytStationLabel =
    "bytstation"


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Beat now ->
            let
                ( nyttLäge, cmd ) =
                    beräknaNyttLäge model.läge model.now model.startTid

                nyStartTid =
                    beräknaNyStartTid model.läge model.now model.startTid
            in
            ( { model | now = now, läge = nyttLäge, startTid = nyStartTid }, cmd )

        StartClick ->
            ( { model | startTid = model.now, läge = Träning }, audioplay (Encode.string startLabel) )

        StopClick ->
            ( { model | startTid = model.now, läge = InnanStart }, Cmd.none )


beräknaNyttLäge : Läge -> Time.Posix -> Time.Posix -> ( Läge, Cmd msg )
beräknaNyttLäge läge now start =
    case läge of
        InnanStart ->
            ( InnanStart, Cmd.none )

        Träning ->
            if (Time.posixToMillis now - Time.posixToMillis start) > träningstid then
                ( Vila, audioplay (Encode.string bytStationLabel) )

            else
                ( Träning, Cmd.none )

        Vila ->
            if (Time.posixToMillis now - Time.posixToMillis start) > vilotid then
                ( Träning, audioplay (Encode.string startLabel) )

            else
                ( Vila, Cmd.none )


beräknaNyStartTid : Läge -> Time.Posix -> Time.Posix -> Time.Posix
beräknaNyStartTid läge now start =
    case läge of
        InnanStart ->
            start

        Träning ->
            if (Time.posixToMillis now - Time.posixToMillis start) > träningstid then
                now

            else
                start

        Vila ->
            if (Time.posixToMillis now - Time.posixToMillis start) > vilotid then
                now

            else
                start



---- VIEW ----


view : Model -> Html Message
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Cirkelträning" ]
        , if model.läge /= InnanStart then
            p [] [ text <| String.fromInt <| (Time.posixToMillis model.now - Time.posixToMillis model.startTid) // 1000 ]

          else
            p [] [ text "Tryck start för att börja träna." ]
        , if model.läge == InnanStart then
            button [ onClick StartClick ] [ text "Start" ]

          else
            button [ onClick StopClick ] [ text "Stopp" ]
        , p [] [ text <| lägeTillText model.läge ]
        , audio [ Html.Attributes.id bytStationLabel ]
            [ source [ src "bytstation.m4a" ] []
            ]
        , audio [ Html.Attributes.id startLabel ]
            [ source [ src "start.m4a" ] []
            ]
        ]


lägeTillText : Läge -> String
lägeTillText läge =
    case läge of
        InnanStart ->
            "Innan start"

        Träning ->
            "Träna!"

        Vila ->
            "Vila."


ljudBaseratPåLäge : Läge -> Html Message
ljudBaseratPåLäge läge =
    case läge of
        InnanStart ->
            span [] []

        Träning ->
            ljud "start.m4a"

        Vila ->
            ljud "bytstation.m4a"


ljud : String -> Html Message
ljud ljudfil =
    audio [ autoplay True ]
        [ source [ src ljudfil ] []
        ]



-- Subscriptions --


subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 500 Beat



---- PROGRAM ----


main : Program () Model Message
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
