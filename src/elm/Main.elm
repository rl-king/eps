module Main exposing (main)

import Browser
import Browser.Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , css
        , placeholder
        , style
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Markdown
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
      , searchTerm = "(a -> b) -> Maybe a -> Maybe b"
      }
    , requestSearchTerm "(a -> b) -> Maybe a -> Maybe b"
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
    , body = [ toUnstyled <| viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    main_ [ css styling.main ]
        [ global globalStyling
        , viewHeader model
        , viewResults model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ css styling.header ]
        [ h1 [ css styling.title ] [ text "eps" ]
        , input
            [ onInput OnSearchTermInput
            , css styling.input
            , autofocus True
            , value model.searchTerm
            ]
            []
        , button [ onClick PerformSearch, css styling.button ] [ text "search" ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    ul [ css styling.searchResults ] <|
        List.map viewResult model.searchResults


viewResult : SearchResult -> Html Msg
viewResult result =
    div [ css styling.searchResult ]
        [ header [ css styling.searchResultHeader ]
            [ viewResultSignature result
            , viewResultCategory result.category
            ]
        , div [ css styling.searchResultBody ]
            [ span [ css styling.searchResultDescription ]
                [ fromUnstyled <| Markdown.toHtml [] result.valueComment ]
            , footer [ css styling.searchResultFooter ]
                [ span [ css styling.searchResultPackageName ] [ text result.packageName ]
                , span [ css styling.searchResultModuleName ] [ text result.moduleName ]
                ]
            ]
        ]


viewResultSignature : SearchResult -> Html Msg
viewResultSignature result =
    code [ css styling.searchResultSignature ]
        [ span [ css styling.searchResultValueName ] [ text result.valueName ]
        , text " : "
        , text result.typeSignature
        ]


viewResultCategory : Category -> Html Msg
viewResultCategory category =
    span
        [ css styling.searchResultCategory
        , style "background-color" (categoryToColor category)
        ]
        [ text (categoryToString category)
        ]



-- STYLING


colors =
    { lightGrey = hex "fafafa"
    , grey = hex "eee"
    , darkGrey = hex "5A6378"
    , white = hex "fff"
    , blue = hex "#005eff"
    , red = hex "#ff3636"
    }


styling =
    { main =
        [ Breakpoint.small []
        ]
    , title =
        [ fontWeight (int 500)
        , fontSize (rem 1)
        , margin zero
        , height (rem 2)
        ]
    , input =
        [ width (pct 100)
        , border3 (px 1) solid colors.grey
        , height (rem 2.5)
        , margin3 (rem 1) zero zero
        , fontSize (rem 1.25)
        , padding2 zero (rem 0.5)
        ]
    , header =
        [ width (pct 100)
        , padding (rem 1)
        , backgroundColor colors.lightGrey
        , height (pct 100)
        , position sticky
        , top (rem -3)
        ]
    , searchResults =
        [ padding (rem 1)
        , width (pct 100)
        ]
    , searchResult =
        [ padding2 (rem 0.5) zero
        , borderBottom3 (px 1) solid colors.grey
        ]
    , searchResultHeader =
        [ padding2 (rem 0.5) zero

        -- , backgroundColor colors.grey
        , displayFlex
        , justifyContent spaceBetween
        ]
    , searchResultBody =
        []
    , searchResultDescription =
        [ fontSize (rem 0.875)
        , color colors.darkGrey
        ]
    , searchResultFooter =
        [ displayFlex
        , justifyContent spaceBetween
        , paddingTop (rem 0.5)
        ]
    , searchResultSignature =
        [ fontSize (rem 1)
        , color colors.blue
        ]
    , searchResultValueName =
        [ color colors.red
        ]
    , searchResultPackageName =
        [ fontWeight (int 500)
        , fontSize (rem 1)
        ]
    , searchResultModuleName =
        [ fontWeight (int 500)
        , fontSize (rem 1)
        ]
    , searchResultCategory =
        [ padding2 (rem 0.15) (rem 0.5)
        ]
    , button =
        [ backgroundColor transparent
        , border zero
        , fontSize (rem 1.25)
        , backgroundColor colors.darkGrey
        , color colors.white
        , height (rem 2)
        , padding2 zero (rem 1)
        , display none
        ]
    }


globalStyling =
    [ Global.everything
        [ boxSizing borderBox ]
    , Global.html
        [ margin zero
        , padding zero
        ]
    , Global.body
        [ margin zero
        , padding zero
        , fontSize (pct 87.5)
        , fontFamilies
            [ "-apple-system"
            , "BlinkMacSystemFont"
            , "Segoe UI"
            , "Roboto"
            , "Oxygen"
            , "Ubuntu"
            , "Cantarell"
            , "Fira Sans"
            , "Droid Sans"
            , "Helvetica Neue"
            , "sans-serif"
            ]
        ]
    , Global.ul
        [ listStyle none
        , padding zero
        , margin zero
        ]
    , Global.code
        [ fontSize (rem 0.875)
        , lineHeight (rem 1.5)
        , padding zero
        , fontFamilies
            [ "Iosevka SS08 Web"
            , "monospace"
            ]
        ]
    , Global.p [ margin3 (rem 0.15) zero (rem 0.25) ]
    , Global.pre [ display none ]
    , Global.img
        [ maxWidth (rem 35)
        , width (pct 100)
        , margin2 (rem 1) zero
        ]
    ]



-- DATA


type alias SearchResult =
    { category : Category
    , packageName : String
    , moduleName : String
    , valueName : String
    , valueComment : String
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
    Decode.map6 SearchResult
        (Decode.field "category" decodeCategory)
        (Decode.field "packageName" Decode.string)
        (Decode.field "moduleName" Decode.string)
        (Decode.field "valueName" Decode.string)
        (Decode.field "valueComment" Decode.string)
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


categoryToString : Category -> String
categoryToString c =
    case c of
        Package ->
            "Package"

        Module ->
            "Module"

        CustomType ->
            "Custom Type"

        TypeAlias ->
            "Type Alias"

        Value ->
            "Value"

        BinOp ->
            "Binary operator"


categoryToColor : Category -> String
categoryToColor c =
    case c of
        -- blue
        Package ->
            "#3CA5EA"

        -- green
        Module ->
            "#43DCC1"

        -- red
        CustomType ->
            "#FD3740"

        -- purple
        TypeAlias ->
            "#7F63D2"

        --yellow
        Value ->
            "#F1D027"

        -- pink
        BinOp ->
            "#f9b2e1"
