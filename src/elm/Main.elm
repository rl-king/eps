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
import Process
import String.Interpolate exposing (interpolate)
import Task
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
    , searchResults : Maybe (Result Http.Error (List SearchResult))
    , searchTerm : String
    , bounce : Maybe Int
    }


init : flags -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    ( { key = key
      , searchResults = Nothing
      , searchTerm = "Task core perform"
      , bounce = Nothing
      }
    , requestSearchTerm "Task core perform"
    )



-- UPDATE


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | OnSearchTermInput String
    | PerformSearch Int
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
            case model.bounce of
                Nothing ->
                    ( { model | searchTerm = searchTerm, bounce = Just 0 }
                    , Task.perform (\_ -> PerformSearch 0) (Process.sleep 350)
                    )

                Just id ->
                    ( { model | searchTerm = searchTerm, bounce = Just (id + 1) }
                    , Task.perform (\_ -> PerformSearch (id + 1)) (Process.sleep 450)
                    )

        PerformSearch id ->
            case Maybe.map ((==) id) model.bounce of
                Nothing ->
                    ( model, Cmd.none )

                Just False ->
                    ( model, Cmd.none )

                Just True ->
                    ( { model | bounce = Nothing }
                    , requestSearchTerm model.searchTerm
                    )

        GotSearchResults result ->
            ( { model | searchResults = Just result }, Cmd.none )



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
        , section [ css styling.content ] (viewResults model)
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ css styling.header ]
        [ h1 [ css styling.title ] [ text "EPS beta" ]
        , div []
            [ label [] [ text "Search" ]
            , input
                [ onInput OnSearchTermInput
                , css styling.input
                , autofocus True
                , value model.searchTerm
                , placeholder "(a -> b) -> Maybe a -> Maybe b, ..."
                ]
                []
            ]

        -- , button [ onClick PerformSearch, css styling.button ] [ text "search" ]
        ]


viewResults : Model -> List (Html Msg)
viewResults model =
    case model.searchResults of
        Just (Ok results) ->
            [ div [ css styling.sidebar ]
                [ h2 [] [ text "Packages" ]
                , ul [] <|
                    List.map (\p -> li [] [ link PackageLink p [] [ text p.packageName ] ]) <|
                        List.filter ((==) Package << .category) results
                , h2 [] [ text "Modules" ]
                , ul [] <|
                    List.map (\p -> li [] [ link ModuleLink p [] [ text p.moduleName ] ]) <|
                        List.filter ((==) Module << .category) results
                ]
            , ul [ css styling.searchResults ] <|
                List.map viewResult results
            ]

        Just (Err (Http.BadPayload err _)) ->
            [ Html.Styled.pre [] [ text err ] ]

        _ ->
            [ h2 [] [ text "loading" ] ]


viewResult : SearchResult -> Html Msg
viewResult result =
    case result.category of
        Package ->
            div [ css styling.searchResult ]
                [ header [ css styling.searchResultHeader ]
                    [ link PackageLink result [] [ h3 [] [ text result.packageName ] ] ]
                , div [ css styling.searchResultBody ]
                    [ footer [ css styling.searchResultFooter ]
                        [ viewResultCategory result.category
                        ]
                    , span [ css styling.points ]
                        [ text (String.fromInt result.points) ]
                    ]
                ]

        Module ->
            div [ css styling.searchResult ]
                [ header [ css styling.searchResultHeader ]
                    [ link ModuleLink result [] [ h3 [] [ text result.moduleName ] ]
                    ]
                , div [ css styling.searchResultBody ]
                    [ footer [ css styling.searchResultFooter ]
                        [ viewResultCategory result.category
                        ]
                    , span [ css styling.points ]
                        [ text (String.fromInt result.points) ]
                    ]
                ]

        Expression _ ->
            div [ css styling.searchResult ]
                [ header [ css styling.searchResultHeader ]
                    [ link ValueLink result [] [ viewResultSignature result ] ]
                , div [ css styling.searchResultBody ]
                    [ footer [ css styling.searchResultDefFooter ]
                        [ div []
                            [ link PackageLink
                                result
                                [ css styling.searchResultPackageName ]
                                [ text result.packageName ]
                            , text " "
                            , link ModuleLink
                                result
                                [ css styling.searchResultPackageName ]
                                []
                            ]
                        , viewResultCategory result.category
                        ]
                    , span [ css styling.points ]
                        [ text (String.fromInt result.points) ]
                    ]
                ]



-- SIGNATURE


viewResultSignature : SearchResult -> Html Msg
viewResultSignature result =
    code [ css styling.searchResultSignature ]
        [ span [ css styling.searchResultValueName ]
            [ text result.moduleName
            , text "."
            , text result.valueName
            ]
        , text " : "
        , span [] <|
            List.intersperse (text " ") <|
                List.map (viewSignaturePart result.packageName) <|
                    toSigParts result.typeSignature
        ]


viewSignaturePart : String -> SigPart -> Html Msg
viewSignaturePart packageName part =
    case part of
        Single s ->
            text s

        WithModule s m ->
            span [ css styling.sigPart ]
                [ text s
                , span [ css styling.sigPartModule ] [ text m ]
                ]


type SigPart
    = Single String
    | WithModule String String


toSigParts : String -> List SigPart
toSigParts =
    let
        split s =
            case List.reverse (String.split "." s) of
                [] ->
                    Single s

                defName :: [] ->
                    Single s

                defName :: rest ->
                    WithModule defName (String.join "." (List.reverse rest))
    in
    List.map split << String.words


viewResultCategory : Category -> Html Msg
viewResultCategory category =
    span
        [ css styling.searchResultCategory
        , css [ backgroundColor (categoryToColor category) ]
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
    , green = hex "42C995"
    , yellow = hex "FFD100"
    }


font =
    { mono =
        [ "SF Mono"
        , "monospace"
        ]
    , text =
        [ "-apple-system"
        , "BlinkMacSystemFont"
        , "Helvetica Neue"
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
        , width (rem 15)
        ]
    , header =
        [ width (pct 100)
        , padding (rem 2)
        , height (rem 6)
        , displayFlex
        , margin2 zero auto
        , maxWidth (rem 70)
        ]
    , input =
        [ Breakpoint.small
            [ height (rem 3.5)
            , padding2 (rem 0.15) (rem 1)
            ]
        , width (rem 30)
        , border zero
        , backgroundColor colors.grey
        , height (rem 3)
        , fontSize (ms 1)
        , padding (rem 0.5)
        , property "-webkit-appearance" "none"
        , borderRadius (px 2)
        , fontFamilies font.mono
        ]
    , content =
        [ displayFlex
        , maxWidth (rem 70)
        , margin2 zero auto
        , padding2 (rem 4) (rem 2)
        ]
    , sidebar =
        [ width (rem 15)
        ]
    , searchResults =
        [ flexGrow (int 1)
        , width (calc (pct 100) minus (rem 20))
        ]
    , searchResult =
        [ marginBottom (rem 1)
        , width (pct 100)
        , borderRadius (px 2)
        , border3 (px 1) solid colors.grey
        , padding (rem 1)
        , position relative
        ]
    , searchResultHeader =
        [ displayFlex
        , alignItems center
        ]
    , searchResultBody =
        []
    , searchResultDefFooter =
        [ displayFlex
        , justifyContent spaceBetween
        , alignItems flexEnd
        , marginTop (rem 0.5)
        ]
    , searchResultFooter =
        [ displayFlex
        , justifyContent flexEnd
        , alignItems flexEnd
        ]
    , searchResultSignature =
        [ fontSize (ms 0)
        , fontWeight (int 500)
        ]
    , searchResultValueName =
        [ color colors.blue
        ]
    , searchResultPackageName =
        [ fontSize (ms 0)
        ]
    , searchResultCategory =
        [ padding3 (rem 0.15) (rem 0.5) (rem 0.2)
        , color colors.white
        , borderRadius (px 2)
        ]
    , sigPart =
        [ position relative
        , hover
            [ Global.descendants
                [ Global.span [ display inlineBlock ]
                ]
            ]
        ]
    , sigPartModule =
        [ position absolute
        , top (rem -0.5)
        , left (pct 50)
        , display none
        , backgroundColor colors.green
        , color colors.white
        , padding2 (rem 0.25) (rem 0.75)
        , transform (translate2 (pct -50) (pct -100))
        , borderRadius (px 2)
        , zIndex (int 2)
        ]

    -- , sigPartPackage =
    --     [ whiteSpace noWrap
    --     , color colors.grey
    --     , display inlineBlock
    -- ]
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
    , points =
        [ position absolute
        , right (rem 1)
        , top (rem 1)
        , fontSize (rem 1)
        , color colors.grey
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
        , margin3 zero zero (rem 2)
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
        [ margin3 (rem 0.15) zero (rem 0.5)
        , fontSize (ms 1)
        , fontWeight (int 500)
        ]
    , Global.h3
        [ margin3 (rem 0.15) zero (rem 0.25)
        , fontSize (ms 2)
        , fontWeight (int 500)
        ]
    , Global.pre
        [ padding (rem 2)
        , fontSize (rem 0.75)
        ]
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
    , Global.label
        [ display block
        , marginBottom (rem 1)
        , fontWeight (int 500)
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
    , points : Int
    }


type Category
    = Package
    | Module
    | Expression String



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
    Decode.map7 SearchResult
        (Decode.field "_rCategory" decodeCategory)
        (Decode.field "_rPackageName" Decode.string)
        (Decode.field "_rModuleName" Decode.string)
        (Decode.field "_rValueName" Decode.string)
        (Decode.field "_rValueComment" Decode.string)
        (Decode.field "_rTypeSignature" Decode.string)
        (Decode.field "_rPoints" Decode.int)


decodeCategory : Decode.Decoder Category
decodeCategory =
    Decode.andThen categoryFromString Decode.string


categoryFromString : String -> Decode.Decoder Category
categoryFromString s =
    case s of
        "Package" ->
            Decode.succeed Package

        "Module" ->
            Decode.succeed Module

        x ->
            Decode.succeed (Expression x)


categoryToString : Category -> String
categoryToString c =
    case c of
        Package ->
            "Package"

        Module ->
            "Module"

        Expression x ->
            x


categoryToColor : Category -> Color
categoryToColor c =
    case c of
        Package ->
            colors.yellow

        Module ->
            colors.green

        Expression _ ->
            colors.blue
