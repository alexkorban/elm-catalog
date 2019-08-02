module Main exposing (Flags, Model, categorise, content, init, main, packageCard, update, view)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Html exposing (Html, button, div, node, span, ul)
import Html.Attributes as Attr
import Markdown
import Task


type Msg
    = NoOp
    | SelectSubcat String


type alias Model =
    { categories : Categories
    , packageCount : Int
    , packages : Packages
    , selectedSubcat : String
    }


type alias Package =
    { name : String
    , summary : String
    , license : String
    , versions : List String
    , tags : List String
    }


type alias Categories =
    Dict String (List String)


type alias Packages =
    Dict String (List Package)


type alias Flags =
    List Package



-- Colors


black =
    rgb255 0 0 0


blue =
    rgb255 52 101 164


darkCharcoal =
    rgb255 46 52 54


green =
    rgb255 115 210 22


grey =
    rgb255 211 215 207


lightBlue =
    rgb255 114 159 207


lightCharcoal =
    rgb255 136 138 133


lightGrey =
    rgb255 238 238 236


orange =
    rgb255 242 100 25


panelBgColor =
    rgba255 246 174 45 0.04



-- Helpers


humaniseCat : String -> String
humaniseCat cat =
    case cat of
        "art" ->
            "Art"

        "audio" ->
            "Audio"

        "data" ->
            "Data"

        "dev" ->
            "Development"

        "game-dev" ->
            "Game development"

        "networking" ->
            "Networking"

        "sciences" ->
            "Sciences"

        "security" ->
            "Security"

        "storage" ->
            "Storage"

        "ui" ->
            "UI"

        s ->
            "UNKNOWN CATEGORY " ++ s


humaniseSubcat : String -> String
humaniseSubcat subcat =
    case subcat of
        "art/interactive-fiction" ->
            "Interactive fiction"

        "audio/integrations" ->
            "Integrations"

        "data/formats" ->
            "Formats"

        "data/random" ->
            "Randomness"

        "data/text" ->
            "Text processing"

        "data/time" ->
            "Date/time"

        "data/visualisation" ->
            "Visualisation"

        "dev/algorithms" ->
            "Algorithms/data structures"

        "dev/code-organisation" ->
            "Code organisation"

        "dev/prototyping" ->
            "Prototyping"

        "dev/parsing" ->
            "Parsing"

        "dev/performance" ->
            "Performance"

        "dev/testing" ->
            "Testing"

        "game-dev/input" ->
            "Input"

        "game-dev/physics" ->
            "Physics"

        "game-dev/procedural-generation" ->
            "Procedural generation"

        "game-dev/rendering" ->
            "Rendering"

        "networking/http" ->
            "HTTP"

        "networking/integrations" ->
            "Integrations"

        "networking/websockets" ->
            "WebSockets"

        "sciences/geography" ->
            "Geography"

        "sciences/maths" ->
            "Maths"

        "sciences/physics" ->
            "Physics"

        "security/crypto" ->
            "Cryptography"

        "storage/databases" ->
            "Databases"

        "storage/localstorage" ->
            "LocalStorage"

        "ui/accessibility" ->
            "Accessibility"

        "ui/animation" ->
            "Animation"

        "ui/colors" ->
            "Colours"

        "ui/css" ->
            "CSS"

        "ui/frameworks" ->
            "Frameworks"

        "ui/html" ->
            "HTML"

        "ui/icons" ->
            "Icons"

        "ui/input" ->
            "Input"

        "ui/i18n" ->
            "Internationalisation"

        "ui/maps" ->
            "Maps"

        "ui/patterns" ->
            "UI widgets & patterns"

        "ui/rendering" ->
            "Rendering"

        "ui/svg" ->
            "SVG"

        "ui/validation" ->
            "Validation"

        s ->
            "UNKNOWN SUBCATEGORY " ++ s


categorise : List Package -> Packages
categorise packages =
    let
        addPackage package maybePackages =
            case maybePackages of
                Nothing ->
                    Nothing

                Just p ->
                    Just <| List.append p [ package ]

        addTag package tag categories =
            if Dict.member tag categories then
                Dict.update tag (addPackage package) categories

            else
                Dict.insert tag [ package ] categories

        categoriseTags package categories =
            List.foldl (addTag package) categories package.tags
    in
    List.foldl categoriseTags Dict.empty packages
        |> Dict.map (\_ p -> List.sortBy .name p)


tagCategory : String -> String
tagCategory tag =
    String.split "/" tag
        |> List.head
        |> Maybe.withDefault "UNKNOWN CATEGORY"


tagSubcategory : String -> String
tagSubcategory tag =
    String.split "/" tag
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault "UNKNOWN SUBCATEGORY"


extractCategories : Packages -> Categories
extractCategories categories =
    let
        appendSubcat subcat =
            Maybe.map (\subcats -> List.append subcats [ subcat ])

        addSubcat tag taxonomy =
            if Dict.member (tagCategory tag) taxonomy then
                Dict.update (tagCategory tag) (appendSubcat <| tag) taxonomy

            else
                Dict.insert (tagCategory tag) [ tag ] taxonomy
    in
    List.foldl addSubcat Dict.empty <| Dict.keys categories


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Browser.Dom.setViewport 0 0)



-- UI


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        packages =
            categorise flags

        categories =
            extractCategories packages

        selection =
            Dict.keys categories
                |> List.head
                |> Maybe.andThen (\key -> Dict.get key categories)
                |> Maybe.andThen List.head
                |> Maybe.withDefault "Interactive fiction"

        packageCount =
            flags
                |> List.filterMap (.tags >> List.head)
                |> List.length
    in
    ( { categories = categories, packageCount = packageCount, packages = packages, selectedSubcat = selection }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectSubcat subcat ->
            ( { model | selectedSubcat = subcat }, resetViewport )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


scrollbarYEl : List (Attribute msg) -> Element msg -> Element msg
scrollbarYEl attrs body =
    el [ height fill, width fill ] <|
        el
            ([ htmlAttribute <| Attr.style "position" "absolute"
             , htmlAttribute <| Attr.style "top" "0"
             , htmlAttribute <| Attr.style "right" "0"
             , htmlAttribute <| Attr.style "bottom" "0"
             , htmlAttribute <| Attr.style "left" "0"
             , Element.scrollbarY
             ]
                ++ attrs
            )
            body


navBar : Int -> Element msg
navBar packageCount =
    row
        [ width fill
        , padding 10
        , Background.color panelBgColor
        , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        , Border.color orange
        ]
        [ link [ centerY ]
            { url = "https://korban.net/elm/catalog"
            , label = image [ width (px 46), height (px 50) ] { src = "https://korban.net/img/logo.png", description = "Korban.net" }
            }
        , el [ padding 10, centerY, Font.color darkCharcoal, Font.size 26, headingTypeface ] <| text "Elm Package Catalog"
        , el
            [ paddingEach { left = 20, top = 7, right = 0, bottom = 0 }
            , centerY
            , Font.color lightCharcoal
            , Font.size 16
            ]
          <|
            text (String.fromInt packageCount ++ " packages")
        , link [ centerY, alignRight, Font.color blue, Font.underline ] { url = "https://korban.net/elm/book", label = text "Practical Elm book" }
        ]


packageCard : Package -> Element Msg
packageCard package =
    column
        [ width <| maximum 800 fill
        , padding 10
        , spacingXY 0 10
        , Border.width 1
        , Border.rounded 3
        , Border.color lightBlue
        , Border.shadow { offset = ( 4, 4 ), blur = 0.1, color = black, size = 2 }
        ]
        [ el [ Font.size 20, Font.color blue, headingTypeface ] <|
            link [] { url = "https://package.elm-lang.org/packages/" ++ package.name ++ "/latest/", label = text package.name }
        , el [ height <| px 1, width fill, Background.color lightGrey ] none
        , paragraph [ Font.size 18 ] <| [ text package.summary ]
        , row [ Font.size 14, Font.color lightCharcoal ]
            [ link [ Font.color lightBlue ] { url = "https://github.com/" ++ package.name, label = text "Source" }
            , text " • "
            , link [ Font.color lightBlue ] { url = "https://elm-greenwood.com/?" ++ String.replace "/" "=" package.name, label = text "Releases" }
            , text " • "
            , text package.license
            ]
        ]


categoryList : Model -> Element Msg
categoryList model =
    let
        catEls ( cat, subcats ) =
            column [ spacingXY 0 10, Font.size 18 ]
                ([ el [ Font.size 22, Font.color darkCharcoal, headingTypeface ] <| text <| humaniseCat cat ]
                    ++ List.map subcatEl (subcats |> List.sortBy humaniseSubcat)
                )

        subcatEl subcat =
            if subcat == model.selectedSubcat then
                el [ Font.color green ] <| text <| humaniseSubcat subcat

            else
                el [ Font.color blue, pointer, onClick (SelectSubcat subcat) ] <| text <| humaniseSubcat subcat
    in
    Dict.toList model.categories
        |> List.map catEls
        |> column [ width fill, height fill, spacingXY 0 10, htmlAttribute <| Attr.style "flex-shrink" "1" ]


content : Model -> Element Msg
content model =
    let
        packageEls =
            model.packages
                |> Dict.get model.selectedSubcat
                |> Maybe.withDefault []
                |> List.map packageCard
    in
    row [ width fill, height fill, spacingXY 10 0, paddingXY 10 10, htmlAttribute <| Attr.style "flex-shrink" "1" ]
        [ el
            [ width <| fillPortion 2
            , height fill
            , scrollbarY
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            , Border.widthEach { right = 2, left = 0, bottom = 0, top = 0 }
            , Border.color lightGrey
            ]
          <|
            categoryList model
        , column
            [ width <| fillPortion 8
            , height fill
            , spacingXY 0 10
            , alignTop
            , scrollbarY
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            ]
            (packageEls
                ++ [ el [ height <| px 30 ] none
                   , paragraph [ Font.size 18 ]
                        [ text "Found a mistake or a missing package?"
                        , text " Drop me a line "
                        , link [ Font.color blue ] { url = "https://korban.net/elm/contact", label = text "by email" }
                        , text " or tweet "
                        , link [ Font.color blue ] { url = "https://twitter.com/alexkorban", label = text "@alexkorban" }
                        , text "."
                        ]
                   , el [ height <| px 30 ] none
                   , bookFooter
                   ]
            )
        ]


markdown : String -> Element msg
markdown s =
    let
        mdOptions : Markdown.Options
        mdOptions =
            { defaultHighlighting = Just "elm"
            , githubFlavored = Just { tables = False, breaks = False }
            , sanitize = False
            , smartypants = True
            }
    in
    el [ width (fill |> maximum 600), Font.size 18 ] <|
        Element.html <|
            Markdown.toHtmlWith mdOptions
                [ Attr.style "white-space" "normal", Attr.style "line-height" "1.5" ]
                s


bookFooterContent =
    """
My book, [Practical Elm for a Busy Developer](https://korban.net/elm/book), skips the basics and gets
right into explaining how to do practical stuff. 

Things like building out the UI, communicating with servers, parsing JSON, structuring 
the application as it grows, testing, and so on. No handholding &mdash; the focus is on 
giving you more substance.

It's up to date with Elm 0.19.

Pop in your email to get a sample chapter.

(You will also get notifications of new posts along with other mailing list only freebies.)
    """


bookFooter : Element Msg
bookFooter =
    row
        [ paddingXY 20 20
        , spacing 30
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        , Background.color <| panelBgColor
        , Border.color <| orange
        , width <| maximum 800 fill
        ]
        [ column
            [ spacing 20, width (fillPortion 6) ]
            [ el [ htmlAttribute <| Attr.attribute "data-drip-attribute" "headline" ] <|
                paragraph [ Font.size 24, headingTypeface, spacingXY 10 10 ]
                    [ text "Looking to build non-trivial apps in Elm?\nThis book will help you." ]
            , markdown bookFooterContent
            ]
        , column
            [ paddingXY 20 10
            , spacing 5
            , height fill
            , width (fillPortion 4)
            , Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
            , Border.color <| lightGrey
            ]
            [ image [ width (px 156), height (px 250), centerX ] { src = "https://korban.net/img/elm-cover.jpg", description = "Book cover" }
            , el [ width fill ] <|
                html <|
                    Html.form
                        [ Attr.action "https://app.convertkit.com/forms/998712/subscriptions"
                        , Attr.class "seva-form formkit-form"
                        , Attr.attribute "data-format" "inline"
                        , Attr.attribute "data-options" "{\"settings\":{\"after_subscribe\":{\"action\":\"message\",\"success_message\":\"Check your email\\nfor the download link.\",\"redirect_url\":\"https://korban.net\"},\"modal\":{\"trigger\":null,\"scroll_percentage\":null,\"timer\":null,\"devices\":null,\"show_once_every\":null},\"recaptcha\":{\"enabled\":false},\"return_visitor\":{\"action\":\"show\",\"custom_content\":\"\"},\"slide_in\":{\"display_in\":null,\"trigger\":null,\"scroll_percentage\":null,\"timer\":null,\"devices\":null,\"show_once_every\":null}}}"
                        , Attr.attribute "data-sv-form" "998712"
                        , Attr.attribute "data-uid" "8b09e227e0"
                        , Attr.attribute "data-version" "5"
                        , Attr.method "post"
                        , Attr.attribute "min-width" "400 500 600 700 800"
                        ]
                        [ div [ Attr.attribute "data-style" "clean" ]
                            [ ul [ Attr.class "formkit-alert formkit-alert-error", Attr.attribute "data-element" "errors", Attr.attribute "data-group" "alert" ]
                                []
                            , div [ Attr.class "seva-fields formkit-fields", Attr.attribute "data-element" "fields", Attr.attribute "data-stacked" "false" ]
                                [ div [ Attr.class "formkit-field" ]
                                    [ Html.input [ Attr.class "formkit-input", Attr.name "email_address", Attr.placeholder "Your email address", Attr.attribute "required" "", Attr.attribute "style" "border-color: rgb(227, 227, 227); border-top-left-radius: 4px; border-top-right-radius: 4px; border-bottom-right-radius: 4px; border-bottom-left-radius: 4px; color: rgb(54, 54, 54); font-weight: 700;", Attr.type_ "email" ]
                                        []
                                    ]
                                , button [ Attr.class "formkit-submit formkit-submit", Attr.attribute "data-element" "submit", Attr.attribute "style" "background-color: rgb(96, 200, 85); border-top-left-radius: 4px; border-top-right-radius: 4px; border-bottom-right-radius: 4px; border-bottom-left-radius: 4px; color: rgb(255, 255, 255); font-weight: 700;" ]
                                    [ div [ Attr.class "formkit-spinner" ]
                                        [ div []
                                            []
                                        , div []
                                            []
                                        , div []
                                            []
                                        ]
                                    , span []
                                        [ Html.text "Send me a sample chapter" ]
                                    ]
                                ]
                            ]
                        , node "style"
                            []
                            [ Html.text ".formkit-form[data-uid=\"8b09e227e0\"] *{font-family:\"Helvetica Neue\",Helvetica,Arial,Verdana,sans-serif;box-sizing:border-box;}.formkit-form[data-uid=\"8b09e227e0\"]{-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;}.formkit-form[data-uid=\"8b09e227e0\"] legend{border:none;font-size:inherit;margin-bottom:10px;padding:0;position:relative;display:table;}.formkit-form[data-uid=\"8b09e227e0\"] fieldset{border:0;padding:0.01em 0 0 0;margin:0;min-width:0;}.formkit-form[data-uid=\"8b09e227e0\"] body:not(:-moz-handler-blocked) fieldset{display:table-cell;}.formkit-form[data-uid=\"8b09e227e0\"] h1,.formkit-form[data-uid=\"8b09e227e0\"] h2,.formkit-form[data-uid=\"8b09e227e0\"] h3,.formkit-form[data-uid=\"8b09e227e0\"] h4,.formkit-form[data-uid=\"8b09e227e0\"] h5,.formkit-form[data-uid=\"8b09e227e0\"] h6{color:inherit;font-size:inherit;font-weight:inherit;}.formkit-form[data-uid=\"8b09e227e0\"] p{color:inherit;font-size:inherit;font-weight:inherit;}.formkit-form[data-uid=\"8b09e227e0\"][data-format=\"modal\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"][data-format=\"slide in\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-select,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-checkboxes{width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit{border:0;border-radius:5px;color:#ffffff;cursor:pointer;display:inline-block;text-align:center;font-size:15px;font-weight:500;cursor:pointer;margin-bottom:15px;overflow:hidden;padding:0;position:relative;vertical-align:middle;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:hover,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:hover,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:focus,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:focus{outline:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:hover > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:hover > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:focus > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:focus > span{background-color:rgba(0,0,0,0.1);}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit > span{display:block;-webkit-transition:all 300ms ease-in-out;transition:all 300ms ease-in-out;padding:12px 24px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input{background:#ffffff;font-size:15px;padding:12px;border:1px solid #e3e3e3;-webkit-flex:1 0 auto;-ms-flex:1 0 auto;flex:1 0 auto;line-height:1.4;margin:0;-webkit-transition:border-color ease-out 300ms;transition:border-color ease-out 300ms;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input:focus{outline:none;border-color:#1677be;-webkit-transition:border-color ease 300ms;transition:border-color ease 300ms;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::-webkit-input-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::-moz-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input:-ms-input-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"]{position:relative;display:inline-block;width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"]::before{content:\"\";top:calc(50% - 2.5px);right:10px;position:absolute;pointer-events:none;border-color:#4f4f4f transparent transparent transparent;border-style:solid;border-width:6px 6px 0 6px;height:0;width:0;z-index:999;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"] select{height:auto;width:100%;cursor:pointer;color:#333333;line-height:1.4;margin-bottom:0;padding:0 6px;-webkit-appearance:none;-moz-appearance:none;appearance:none;font-size:15px;padding:12px;padding-right:25px;border:1px solid #e3e3e3;background:#ffffff;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"] select:focus{outline:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"]{text-align:left;margin:0;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"]{margin-bottom:10px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] *{cursor:pointer;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"]:last-of-type{margin-bottom:0;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"] + label::after{content:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]:checked + label::after{border-color:#ffffff;content:\"\";}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]:checked + label::before{background:#10bf7a;border-color:#10bf7a;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label{position:relative;display:inline-block;padding-left:28px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::before,.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::after{position:absolute;content:\"\";display:inline-block;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::before{height:16px;width:16px;border:1px solid #e3e3e3;background:#ffffff;left:0px;top:3px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::after{height:4px;width:8px;border-left:2px solid #4d4d4d;border-bottom:2px solid #4d4d4d;-webkit-transform:rotate(-45deg);-ms-transform:rotate(-45deg);transform:rotate(-45deg);left:4px;top:8px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert{background:#f9fafb;border:1px solid #e3e3e3;border-radius:5px;-webkit-flex:1 0 auto;-ms-flex:1 0 auto;flex:1 0 auto;list-style:none;margin:25px auto;padding:12px;text-align:center;width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert:empty{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert-success{background:#d3fbeb;border-color:#10bf7a;color:#0c905c;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert-error{background:#fde8e2;border-color:#f2643b;color:#ea4110;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner{display:-webkit-box;display:-webkit-flex;display:-ms-flexbox;display:flex;height:0px;width:0px;margin:0 auto;position:absolute;top:0;left:0;right:0;width:0px;overflow:hidden;text-align:center;-webkit-transition:all 300ms ease-in-out;transition:all 300ms ease-in-out;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div{margin:auto;width:12px;height:12px;background-color:#fff;opacity:0.3;border-radius:100%;display:inline-block;-webkit-animation:formkit-bouncedelay-formkit-form-data-uid-8b09e227e0- 1.4s infinite ease-in-out both;animation:formkit-bouncedelay-formkit-form-data-uid-8b09e227e0- 1.4s infinite ease-in-out both;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div:nth-child(1){-webkit-animation-delay:-0.32s;animation-delay:-0.32s;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div:nth-child(2){-webkit-animation-delay:-0.16s;animation-delay:-0.16s;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit[data-active] .formkit-spinner{opacity:1;height:100%;width:50px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit[data-active] .formkit-spinner ~ span{opacity:0;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-powered-by[data-active=\"false\"]{opacity:0.35;}@-webkit-keyframes formkit-bouncedelay-formkit-form-data-uid-8b09e227e0-{0%,80%,100%{-webkit-transform:scale(0);-ms-transform:scale(0);transform:scale(0);}40%{-webkit-transform:scale(1);-ms-transform:scale(1);transform:scale(1);}}@keyframes formkit-bouncedelay-formkit-form-data-uid-8b09e227e0-{0%,80%,100%{-webkit-transform:scale(0);-ms-transform:scale(0);transform:scale(0);}40%{-webkit-transform:scale(1);-ms-transform:scale(1);transform:scale(1);}} .formkit-form[data-uid=\"8b09e227e0\"]{max-width:700px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-style=\"clean\"]{width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-fields{display:-webkit-box;display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-wrap:wrap;-ms-flex-wrap:wrap;flex-wrap:wrap;margin:0 auto;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit{margin:0 0 15px 0;-webkit-flex:1 0 100%;-ms-flex:1 0 100%;flex:1 0 100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-powered-by{color:#7d7d7d;display:block;font-size:12px;margin:0;text-align:center;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] [data-style=\"clean\"],.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] [data-style=\"clean\"]{padding:10px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"],.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"]{margin-left:-5px;margin-right:-5px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit{margin:0 5px 15px 5px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-field{-webkit-flex:100 1 auto;-ms-flex:100 1 auto;flex:100 1 auto;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit{-webkit-flex:1 1 auto;-ms-flex:1 1 auto;flex:1 1 auto;} .formkit-form[data-uid=\"8b09e227e0\"] input{border:2px solid #e0e0e0;font-family:\"Source Serif Pro\",Georgia,\"Times New Roman\",sans-serif;font-size:18px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit span{box-sizing:border-box;font-family:\"Inconsolata\",monospace;border:2px solid #3f8438;}" ]
                        ]
            ]
        ]


baseTypeface : Element.Attribute msg
baseTypeface =
    Font.family [ Font.typeface "Helvetica", Font.typeface "Georgia", Font.typeface "Times New Roman", Font.serif ]


headingTypeface : Element.Attribute msg
headingTypeface =
    Font.family [ Font.typeface "Patua One", Font.typeface "Helvetica", Font.sansSerif ]


view : Model -> Html Msg
view model =
    layout [ height fill, baseTypeface ] <|
        column [ width fill, height fill, spacingXY 0 20, htmlAttribute <| Attr.style "flex-shrink" "1" ]
            [ navBar model.packageCount
            , content model
            ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "Elm Package Catalog", body = [ view model ] }
        , subscriptions = subscriptions
        }
