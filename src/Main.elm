module Main exposing (Message(..), Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time



---- MODEL ----


type alias Model =
    { now : Time.Posix
    , startClicked : Time.Posix
    }


init : ( Model, Cmd Message )
init =
    ( { now = Time.millisToPosix 0
      , startClicked = Time.millisToPosix 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Message
    = Beat Time.Posix
    | StartClick


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Beat now ->
            ( { model | now = now }, Cmd.none )

        StartClick ->
            ( { model | startClicked = model.now }, Cmd.none )



---- VIEW ----


view : Model -> Html Message
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Förfluten tid" ]
        , p [] [ text <| String.fromInt <| (Time.posixToMillis model.now - Time.posixToMillis model.startClicked) // 1000 ]
        , button [ onClick StartClick ] [ text "Start" ]
        ]



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
