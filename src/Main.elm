module Main exposing (Message(..), Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time



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


type Läge
    = InnanStart
    | Träning
    | Vila


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Beat now ->
            let
                nyttLäge =
                    beräknaNyttLäge model.läge model.now model.startTid

                nyStartTid =
                    beräknaNyStartTid model.läge model.now model.startTid
            in
            ( { model | now = now, läge = nyttLäge, startTid = nyStartTid }, Cmd.none )

        StartClick ->
            ( { model | startTid = model.now, läge = Träning }, Cmd.none )


beräknaNyttLäge : Läge -> Time.Posix -> Time.Posix -> Läge
beräknaNyttLäge läge now start =
    case läge of
        InnanStart ->
            InnanStart

        Träning ->
            if (Time.posixToMillis now - Time.posixToMillis start) > 60000 then
                Vila

            else
                Träning

        Vila ->
            if (Time.posixToMillis now - Time.posixToMillis start) > 10000 then
                Träning

            else
                Vila


beräknaNyStartTid : Läge -> Time.Posix -> Time.Posix -> Time.Posix
beräknaNyStartTid läge now start =
    case läge of
        InnanStart ->
            start

        Träning ->
            if (Time.posixToMillis now - Time.posixToMillis start) > 60000 then
                now

            else
                start

        Vila ->
            if (Time.posixToMillis now - Time.posixToMillis start) > 10000 then
                now

            else
                start



---- VIEW ----


view : Model -> Html Message
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Förfluten tid" ]
        , p [] [ text <| String.fromInt <| (Time.posixToMillis model.now - Time.posixToMillis model.startTid) // 1000 ]
        , button [ onClick StartClick ] [ text "Start" ]
        , p [] [ text <| lägeTillText model.läge ]
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
    Time.every 1000 Beat



---- PROGRAM ----


main : Program () Model Message
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
