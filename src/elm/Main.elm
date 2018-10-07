module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key }


init : flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    ( { key = key }, Cmd.none )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Browser.Navigation.load href )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = [ main_ [] [ h1 [] [ text "hello world" ] ] ]
    }
