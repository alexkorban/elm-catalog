module Main exposing (Flags, Model, categorise, content, init, main, packageCard, update, view)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Html exposing (Html)
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
        , Border.width 2
        , Border.rounded 4
        , Border.color lightBlue
        , Border.shadow { offset = ( 2, 2 ), blur = 0.1, color = black, size = 2 }
        ]
        [ el [ Font.size 20, Font.color blue, headingTypeface ] <|
            link [] { url = "https://package.elm-lang.org/packages/" ++ package.name ++ "/latest/", label = text package.name }
        , el [ height <| px 1, width fill, Border.width 1, Border.color lightGrey ] none
        , paragraph [ Font.size 18 ] <| [ text package.summary ]
        , row [ Font.size 14, Font.color lightCharcoal ]
            [ link [ Font.color lightBlue ] { url = "https://github.com/" ++ package.name, label = text "GitHub" }
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
                        [ Attr.attribute "action" "https://www.getdrip.com/forms/716327102/submissions"
                        , Attr.attribute "method" "post"
                        , Attr.attribute "data-drip-embedded-form" "716327102"
                        ]
                        [ Html.input
                            [ Attr.attribute "name" "fields[email]"
                            , Attr.type_ "text"
                            , Attr.placeholder "Email"
                            , Attr.style "width" "100%"
                            , Attr.style "display" "block"
                            , Attr.style "box-sizing" "border-box"
                            , Attr.style "margin" "1rem"
                            , Attr.style "padding" "4px"
                            , Attr.style "font-weight" "bold"
                            , Attr.style "border-radius" "3px"
                            , Attr.style "border" "2px solid #33658a"
                            , Attr.style "font-size" "18px"
                            , Attr.style "font-family" "Source Serif Pro, Georgia, Times New Roman, serif"
                            ]
                            []
                        , Html.input
                            [ Attr.attribute "type" "submit"
                            , Attr.attribute "name" "submit"
                            , Attr.attribute "data-drip-attribute" "sign-up-button"
                            , Attr.attribute "value" "Send me a sample chapter"
                            , Attr.style "width" "100%"
                            , Attr.style "display" "block"
                            , Attr.style "border-radius" "4px"
                            , Attr.style "border" "2px solid #3f8438"
                            , Attr.style "padding" "10px"
                            , Attr.style "margin" "1rem"
                            , Attr.style "margin-top" "0"
                            , Attr.style "font-size" "18px"
                            , Attr.style "font-family" "Helvetica, sans-serif"
                            , Attr.style "background-color" "#60c855"
                            , Attr.style "color" "white"
                            , Attr.style "cursor" "pointer"
                            ]
                            []
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
