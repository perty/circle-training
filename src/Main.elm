module Main exposing (Message(..), Model, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (src)
import Time



---- MODEL ----


type alias Model =
    { now : Time.Posix
    }


init : ( Model, Cmd Message )
init =
    ( { now = Time.millisToPosix 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Message
    = Beat Time.Posix


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Beat now ->
            ( { model | now = now }, Cmd.none )



---- VIEW ----


view : Model -> Html Message
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Time is now" ]
        , p [] [ text <| String.fromInt <| Time.posixToMillis model.now ]
        ]



-- Subscriptions --


subscriptions : Model -> Sub Message
subscriptions model =
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
