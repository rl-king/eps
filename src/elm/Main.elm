module Main exposing (main)

import Browser
import Browser.Navigation
import Css exposing (..)
import Css.Breakpoint as Breakpoint
import Css.Global as Global exposing (global)
import Html.Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes
    exposing
        ( attribute
        , autofocus
        , css
        , href
        , placeholder
        , style
        , target
        , type_
        , value
        )
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed as Keyed
import Http
import Json.Decode as Decode
import Markdown
import ModularScale
import String.Interpolate exposing (interpolate)
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
      , searchTerm = "gaming"
      }
    , requestSearchTerm "gaming"
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
            , placeholder "(a -> b) -> Maybe a -> Maybe b, ..."
            ]
            []
        , button [ onClick PerformSearch, css styling.button ] [ text "search" ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    section [ css styling.content ]
        [ div [ css styling.sidebar ]
            [ h2 [] [ text "packages" ]
            , h2 [] [ text "modules" ]
            ]
        , ul [ css styling.searchResults ] <|
            List.map viewResult model.searchResults
        ]


viewResult : SearchResult -> Html Msg
viewResult result =
    div [ css styling.searchResult ]
        [ header [ css styling.searchResultHeader ]
            [ link ValueLink result [] [ viewResultSignature result ]
            ]
        , div [ css styling.searchResultBody ]
            [ fromUnstyled <|
                Markdown.toHtml [ Html.Attributes.class "markdown" ] result.valueComment
            , footer [ css styling.searchResultFooter ]
                [ link PackageLink
                    result
                    [ css styling.searchResultPackageName ]
                    [ text result.packageName ]
                , viewResultCategory result.category
                ]
            ]
        ]


viewResultSignature : SearchResult -> Html Msg
viewResultSignature result =
    code [ css styling.searchResultSignature ]
        [ span [ css styling.searchResultValueName ]
            [ text (result.moduleName ++ "." ++ result.valueName)
            ]
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



-- LINK


baseUrl : String
baseUrl =
    "http://package.elm-lang.org/packages"


type Link
    = PackageLink
    | ModuleLink
    | ValueLink


link : Link -> SearchResult -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
link linkType { packageName, moduleName, valueName } attributes content =
    let
        anchor path segments =
            a ([ href (interpolate path segments), target "_blank" ] ++ attributes) content
    in
    case linkType of
        PackageLink ->
            anchor "{0}/{1}/latest" [ baseUrl, packageName ]

        ModuleLink ->
            anchor "{0}/{1}/latest/{2}" [ baseUrl, packageName, moduleName ]

        ValueLink ->
            anchor "{0}/{1}/latest/{2}#{3}" [ baseUrl, packageName, moduleName, valueName ]



-- STYLING


config : ModularScale.Config
config =
    ModularScale.config [ 1 ] ModularScale.MinorThird


ms : Int -> Rem
ms =
    rem << ModularScale.get config


colors =
    { lightGrey = hex "FAFAFA"
    , grey = hex "EEEEEE"
    , sand = hex "EDE9E0"
    , sandDarker = hex "D8D1C2"
    , darkGrey = hex "5A6378"
    , black = hex "111111"
    , white = hex "FFFFFF"
    , blue = hex "005Eff"
    , red = hex "ff3636"
    , green = hex "349033"
    }


font =
    { mono =
        [ "Iosevka SS08 Web"
        , "monospace"
        ]
    , text =
        [ "Helvetica Neue"
        , "-apple-system"
        , "BlinkMacSystemFont"
        , "Segoe UI"
        , "Roboto"
        , "Oxygen"
        , "Ubuntu"
        , "Cantarell"
        , "Fira Sans"
        , "Droid Sans"
        , "sans-serif"
        ]
    }


styling =
    { main =
        [ Breakpoint.small []
        ]
    , title =
        [ fontWeight (int 500)
        , fontSize (ms 1)
        , margin zero
        , height (rem 2)
        , position absolute
        , left (rem 2)
        , top (rem 2)
        ]
    , header =
        [ width (pct 100)
        , padding (rem 1)
        , backgroundColor colors.sand
        , height (rem 6)
        , displayFlex
        , justifyContent center
        , alignItems center
        ]
    , input =
        [ width (rem 30)
        , border3 (px 1) solid colors.sandDarker
        , height (rem 2)
        , fontSize (ms 1)
        , padding (rem 0.5)
        , property "-webkit-appearance" "none"
        , borderRadius (px 2)
        , fontFamilies font.mono
        , Breakpoint.small
            [ height (rem 2.5)
            , padding2 (rem 0.15) (rem 0.5)
            ]
        ]
    , content =
        [ displayFlex
        , maxWidth (rem 70)
        , margin2 zero auto
        , padding (rem 2)
        ]
    , sidebar =
        [ width (rem 20)
        ]
    , searchResults =
        [ width (pct 100)
        ]
    , searchResult =
        [ marginBottom (rem 1)
        , width (pct 100)
        , borderRadius (px 2)
        ]
    , searchResultHeader =
        [ backgroundColor colors.sand
        , height (ms 6)
        , padding2 zero (rem 1)
        , displayFlex
        , alignItems center
        ]
    , searchResultBody =
        [ padding2 (rem 0.5) (rem 1) ]
    , searchResultFooter =
        [ fontFamilies font.mono
        , fontSize (ms 0)
        , displayFlex
        , justifyContent spaceBetween
        , alignItems flexEnd
        , marginTop (rem 1)
        ]
    , searchResultSignature =
        [ fontSize (ms 0)
        , color colors.red
        , fontWeight (int 500)
        ]
    , searchResultValueName =
        [ color colors.black
        ]
    , searchResultPackageName =
        [ color colors.green
        ]
    , searchResultCategory =
        [ padding2 (rem 0.15) (rem 0.5)
        ]
    , button =
        [ backgroundColor transparent
        , border zero
        , fontSize (ms 1)
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
        , fontSize (pct 87.5)
        ]
    , Global.body
        [ margin zero
        , padding zero
        , fontFamilies font.text
        , fontSize (ms 1)
        ]
    , Global.ul
        [ listStyle none
        , padding zero
        , margin zero
        ]
    , Global.code
        [ fontSize (ms 0)
        , lineHeight (ms 2)
        , padding zero
        , fontFamilies font.mono
        ]
    , Global.p
        [ margin3 (rem 0.15) zero (rem 0.25)
        ]
    , Global.h2
        [ margin3 (rem 0.15) zero (rem 0.25)
        , fontSize (ms 1)
        , fontWeight (int 400)
        ]
    , Global.pre [ display none ]
    , Global.a
        [ textDecoration none
        , color colors.black
        , hover [ textDecoration underline ]
        ]
    , Global.img
        [ maxWidth (rem 5)
        , width (pct 100)
        , margin2 (rem 1) zero
        , display none
        ]
    , Global.class "markdown"
        [ overflow hidden
        , maxHeight (rem 3)
        , display block
        , fontSize (ms 0)
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
