module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
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
    { key : Browser.Navigation.Key
    , searchResults : List String
    , searchTerm : String
    }


init : flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    ( { key = key
      , searchResults = []
      , searchTerm = ""
      }
    , requestAll
    )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnSearchTermInput String
    | PerformSearch
    | GotSearchResults (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlChange location ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Browser.Navigation.load href )

        OnSearchTermInput searchTerm ->
            ( { model | searchTerm = searchTerm }
            , requestSearchTerm model.searchTerm
            )

        PerformSearch ->
            ( model, requestSearchTerm model.searchTerm )

        GotSearchResults (Ok searchResults) ->
            ( { model | searchResults = searchResults }, Cmd.none )

        GotSearchResults (Err err) ->
            let
                _ =
                    Debug.log "Error in search results request" err
            in
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "eps"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    main_ []
        [ h1 [] [ text "eps" ]
        , input [ onInput OnSearchTermInput ] []
        , button [ onClick PerformSearch ] [ text "search" ]
        , ul [] <|
            List.map (\x -> li [] [ text x ]) model.searchResults
        ]



-- REQUEST


requestSearchTerm : String -> Cmd Msg
requestSearchTerm searchTerm =
    Http.send GotSearchResults <|
        Http.get ("/search?term=" ++ searchTerm) decode


requestAll : Cmd Msg
requestAll =
    Http.send GotSearchResults <|
        Http.get "/search" decode


decode : Decode.Decoder (List String)
decode =
    Decode.list (Decode.field "name" Decode.string)
