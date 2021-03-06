port module Main exposing (Message(..), Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, audio, button, div, h1, img, input, label, p, source, text)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Time


port audioplay : Encode.Value -> Cmd msg



---- MODEL ----


type alias Model =
    { now : Time.Posix
    , startTid : Time.Posix
    , läge : Läge
    , aktivTid : Int
    }


init : ( Model, Cmd Message )
init =
    ( { now = Time.millisToPosix 0
      , startTid = Time.millisToPosix 0
      , läge = InnanStart
      , aktivTid = 60
      }
    , Cmd.none
    )



---- UPDATE ----


type Message
    = Beat Time.Posix
    | StartClick
    | StopClick
    | AktivTid String


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

        AktivTid nyTidStr ->
            let
                nyTid =
                    case String.toInt nyTidStr of
                        Just n ->
                            n

                        Nothing ->
                            model.aktivTid
            in
            ( { model | aktivTid = nyTid }, Cmd.none )


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
    let
        huvud =
            if model.läge /= InnanStart then
                p [] [ text <| String.fromInt <| (Time.posixToMillis model.now - Time.posixToMillis model.startTid) // 1000 ]

            else
                p [] [ text "Tryck start för att börja träna." ]

        startStoppKnapp =
            if model.läge == InnanStart then
                button [ onClick StartClick ] [ text "Start" ]

            else
                button [ onClick StopClick ] [ text "Stopp" ]

        lägesText =
            p [] [ text <| lägeTillText model.läge ]

        bytStationLjud =
            audio [ Html.Attributes.id bytStationLabel ]
                [ source [ src "bytstation.m4a" ] []
                ]

        startLjud =
            audio [ Html.Attributes.id startLabel ]
                [ source [ src "start.m4a" ] []
                ]

        inställningar =
            div [ style "display" "flex", style "flex-direction" "row", style "width" "100%", style "justify-content" "center" ]
                [ label [] [ text "Aktiv tid" ]
                , input [ onInput AktivTid, value <| String.fromInt model.aktivTid, type_ "number" ] []
                , label [] [ text "Vilotid" ]
                , input [] []
                ]
    in
    div []
        [ img [ src "%PUBLIC_URL%/logotype.png" ] []
        , h1 [] [ text "Cirkelträning" ]
        , inställningar
        , huvud
        , startStoppKnapp
        , lägesText
        , bytStationLjud
        , startLjud
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
