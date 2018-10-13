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
    , searchResults : List SearchResult
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
    | GotSearchResults (Result Http.Error (List SearchResult))


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
            , requestSearchTerm searchTerm
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
        , label [] [ text "enter at least 3 chars" ]
        , input [ onInput OnSearchTermInput ] []
        , button [ onClick PerformSearch ] [ text "search" ]
        , p [] [ text (String.fromInt <| List.length model.searchResults) ]
        , ul [] <|
            List.map viewResult model.searchResults
        ]


viewResult : SearchResult -> Html Msg
viewResult result =
    div []
        [ header []
            [ label [] [ text "Value" ]
            , h2 [] [ text result.packageName ]
            ]
        , article [] [ text result.typeSignature ]
        ]



-- DATA


type alias SearchResult =
    { category : Category
    , packageName : String
    , moduleName : String
    , valueName : String
    , typeSignature : String
    }


type Category
    = Package
    | Module
    | CustomType
    | TypeAlias
    | Value
    | BinOp



-- HTTP


requestSearchTerm : String -> Cmd Msg
requestSearchTerm searchTerm =
    Http.send GotSearchResults <|
        Http.get ("/search?term=" ++ searchTerm) (Decode.list decodeResult)


requestAll : Cmd Msg
requestAll =
    Http.send GotSearchResults <|
        Http.get "/search" (Decode.list decodeResult)



-- DECODE


decodeResult : Decode.Decoder SearchResult
decodeResult =
    Decode.map5 SearchResult
        (Decode.field "category" decodeCategory)
        (Decode.field "packageName" Decode.string)
        (Decode.field "moduleName" Decode.string)
        (Decode.field "valueName" Decode.string)
        (Decode.field "typeSignature" Decode.string)


decodeCategory : Decode.Decoder Category
decodeCategory =
    Decode.map categoryFromString Decode.string


categoryFromString : String -> Category
categoryFromString s =
    case s of
        "Package" ->
            Package

        "Module" ->
            Module

        "CustomType" ->
            CustomType

        "TypeAlias" ->
            TypeAlias

        "Value" ->
            Value

        _ ->
            BinOp
