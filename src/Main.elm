module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Html exposing (Html, button, div, node, span, ul)
import Html.Attributes as Attr
import Http
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Task
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>))


type Route
    = PackageRoute String
    | ToolRoute String


type Msg
    = RuntimeChangedUrl Url
    | RuntimeDidSomethingIrrelevant
    | RuntimeReceivedReadme PackageName (Result Http.Error Markdown)
    | UserClickedLink UrlRequest
    | UserClickedReadmeButton PackageName
    | UserClickedMenuIcon
    | UserClickedOutsideMenuPanel
    | UserResizedWindow Int Int
    | UserSelectedSubcat String
    | UserSelectedToolCat String


type alias Markdown =
    String


type alias PackageName =
    String


type alias ToolName =
    String


type Tab
    = PackagesTab
    | ToolsTab


type alias Size =
    { height : Int, width : Int }


type alias Model =
    { isMenuPanelOpen : Bool
    , navKey : Nav.Key
    , pkgCategories : PkgCategories
    , packageCount : Int
    , packages : Packages
    , readmes : Readmes
    , route : Route
    , selectedPkgSubcat : String
    , selectedToolCat : String
    , tools : Tools
    , toolCount : Int
    , urlPrefix : String
    , windowSize : Size
    }


type alias Package =
    { name : PackageName
    , summary : String
    , license : String
    , versions : List String
    , tags : List String
    }


type alias Tool =
    { name : ToolName
    , url : Maybe String
    , packageUrl : Maybe String
    , githubName : String
    , summary : String
    , tags : List String
    }


type alias Readme =
    { isOpen : Bool
    , text : Markdown
    }


type alias PkgCategories =
    Dict String (List String)


type alias Packages =
    Dict String (List Package)


type alias Tools =
    Dict String (List Tool)


type alias Readmes =
    Dict PackageName Readme


type alias Flags =
    { packages : List Package
    , tools : List Tool
    , urlPrefix : String
    , windowHeight : Int
    , windowWidth : Int
    }



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


lightGreen =
    rgb255 189 239 139


lightGrey =
    rgb255 238 238 236


orange =
    rgb255 242 100 25


panelBgColor =
    rgba255 246 174 45 0.04


white =
    rgb255 255 255 255



-- Helpers


if_ : Bool -> a -> a -> a
if_ predicate lhs rhs =
    if predicate then
        lhs

    else
        rhs


attrNone =
    htmlAttribute <| Attr.class " "


sides =
    { top = 0, bottom = 0, left = 0, right = 0 }


topLevelParser : String -> UrlParser.Parser a a
topLevelParser urlPrefix =
    case urlPrefix of
        "" ->
            UrlParser.top

        s ->
            s
                |> String.split "/"
                |> List.map UrlParser.s
                |> List.foldr (</>) UrlParser.top



-- UrlParser.oneOf [ UrlParser.top, UrlParser.s "elm" </> UrlParser.s "catalog" ]


routeParser : String -> UrlParser.Parser (Route -> a) a
routeParser urlPrefix =
    let
        top =
            topLevelParser urlPrefix
    in
    UrlParser.oneOf
        [ UrlParser.map (PackageRoute "dev/algorithms") <| top </> UrlParser.s "packages"

        --, UrlParser.map PackageRoute <| top </> UrlParser.s "packages" </> UrlParser.string
        , UrlParser.map (ToolRoute "build") <| top </> UrlParser.s "tools"

        --, UrlParser.map ToolRoute <| top </> UrlParser.s "tools" </> UrlParser.string
        , UrlParser.map (PackageRoute "dev/utils") <| top
        ]


humanisePkgCat : String -> String
humanisePkgCat cat =
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

        "platforms" ->
            "Platforms"

        "sciences" ->
            "Sciences"

        "security" ->
            "Security"

        "storage" ->
            "Storage"

        "ui" ->
            "UI"

        "uncat" ->
            "Uncategorised"

        s ->
            "UNKNOWN CATEGORY " ++ s


humanisePkgSubcat : String -> String
humanisePkgSubcat subcat =
    case subcat of
        "art/generative" ->
            "Generative"

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
            "Parsing/languages"

        "dev/performance" ->
            "Performance"

        "dev/testing" ->
            "Testing"

        "dev/utils" ->
            "Utils"

        "game-dev/code-organisation" ->
            "Code organisation"

        "game-dev/input" ->
            "Input"

        "game-dev/physics" ->
            "Physics"

        "game-dev/procedural-generation" ->
            "Procedural generation"

        "game-dev/rendering" ->
            "Rendering"

        "game-dev/utils" ->
            "Utils"

        "networking/http" ->
            "HTTP"

        "networking/integrations" ->
            "Integrations"

        "networking/websockets" ->
            "WebSockets"

        "platforms/desktop" ->
            "Desktop"

        "platforms/serverless" ->
            "Serverless"

        "platforms/static-sites" ->
            "Static sites"

        "sciences/data-mining" ->
            "Data mining"

        "sciences/geography" ->
            "Geography"

        "sciences/maths" ->
            "Maths"

        "sciences/machine-learning" ->
            "Machine learning"

        "sciences/physics" ->
            "Physics"

        "sciences/statistics" ->
            "Statistics"

        "security/crypto" ->
            "Cryptography"

        "security/other" ->
            "Other"

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

        "ui/elm-ui" ->
            "Elm-ui"

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

        "uncat/new" ->
            "New"

        s ->
            "UNKNOWN SUBCATEGORY " ++ s


humaniseToolCat : String -> String
humaniseToolCat cat =
    case cat of
        "build" ->
            "Build tools"

        "cli" ->
            "CLI"

        "code-analysis" ->
            "Code analysis"

        "code-generation" ->
            "Code & site generation"

        "code-transformation" ->
            "Code transformation"

        "debugging" ->
            "Debugging"

        "dev-servers" ->
            "Dev servers"

        "documentation" ->
            "Documentation"

        "editor" ->
            "IDE/editor tools"

        "haskell" ->
            "Haskell packages"

        "information" ->
            "Information"

        "other" ->
            "Other"

        "package-management" ->
            "Package management"

        "testing" ->
            "Testing"

        "typescript" ->
            "TypeScript"

        s ->
            "UNKNOWN CATEGORY " ++ s


categorise : List { a | name : String, tags : List String } -> Dict String (List { a | name : String, tags : List String })
categorise packages =
    let
        addPackage package maybePackages =
            Maybe.map (\pkgs -> List.append pkgs [ package ]) maybePackages

        addTag package tag categories =
            if Dict.member tag categories then
                Dict.update tag (addPackage package) categories

            else
                Dict.insert tag [ package ] categories

        categoriseTags package categories =
            List.foldl (addTag package) categories package.tags
    in
    List.foldl categoriseTags Dict.empty packages
        |> Dict.map (\_ pkgs -> List.sortBy (.name >> String.toLower) pkgs)


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


extractPkgCategories : Packages -> PkgCategories
extractPkgCategories categories =
    let
        appendSubcat subcat =
            Maybe.map (\subcats -> List.append subcats [ subcat ])

        addSubcat tag taxonomy =
            if Dict.member (tagCategory tag) taxonomy then
                Dict.update (tagCategory tag) (appendSubcat tag) taxonomy

            else
                Dict.insert (tagCategory tag) [ tag ] taxonomy
    in
    List.foldl addSubcat Dict.empty <| Dict.keys categories


resetEntryListViewPort : Cmd Msg
resetEntryListViewPort =
    Task.attempt (\_ -> RuntimeDidSomethingIrrelevant) <| Browser.Dom.setViewportOf "entryList" 0 0


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> RuntimeDidSomethingIrrelevant) <| Browser.Dom.setViewport 0 0


resetStateOnPageChange : Model -> Model
resetStateOnPageChange model =
    { model | isMenuPanelOpen = False }



-- UI
-- TODO: pass in the prefix where app is mounted, then use it to construct the top level
-- URL parser (which now has to be stored in the model)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        packages =
            categorise flags.packages

        pkgCategories =
            extractPkgCategories packages

        selection =
            Dict.get "dev" pkgCategories
                |> Maybe.andThen List.head
                |> Maybe.withDefault "Algorithms/data structures"

        packageCount =
            flags.packages
                |> List.map
                    (\pkg ->
                        case List.head pkg.tags of
                            Just "exclude" ->
                                0

                            Just "uncat/new" ->
                                0

                            Just _ ->
                                1

                            Nothing ->
                                0
                    )
                |> List.sum

        toolCount =
            List.length flags.tools

        tools =
            categorise flags.tools
    in
    ( { isMenuPanelOpen = False
      , navKey = navKey
      , pkgCategories = pkgCategories
      , packageCount = packageCount
      , packages = packages
      , readmes = Dict.empty
      , route =
            Maybe.withDefault (PackageRoute "dev/prototyping") <|
                UrlParser.parse (routeParser flags.urlPrefix) url
      , selectedPkgSubcat = selection
      , selectedToolCat = "build"
      , toolCount = toolCount
      , tools = tools
      , urlPrefix = flags.urlPrefix
      , windowSize = { height = flags.windowHeight, width = flags.windowWidth }
      }
    , Cmd.none
    )


getReadme : PackageName -> Cmd Msg
getReadme packageName =
    Http.get
        { url = "https://raw.githubusercontent.com/" ++ packageName ++ "/master/README.md"
        , expect = Http.expectString <| RuntimeReceivedReadme packageName
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeChangedUrl url ->
            ( { model
                | route =
                    Maybe.withDefault (PackageRoute "dev/testing") <|
                        UrlParser.parse (routeParser model.urlPrefix) url
              }
            , Cmd.none
            )

        RuntimeDidSomethingIrrelevant ->
            ( model, Cmd.none )

        RuntimeReceivedReadme packageName (Ok readmeText) ->
            ( { model | readmes = Dict.insert packageName { isOpen = True, text = readmeText } model.readmes }, Cmd.none )

        RuntimeReceivedReadme _ (Err _) ->
            ( model, Cmd.none )

        UserClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( resetStateOnPageChange model, Nav.pushUrl model.navKey <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        UserClickedMenuIcon ->
            ( { model | isMenuPanelOpen = True }, Cmd.none )

        UserClickedOutsideMenuPanel ->
            ( { model | isMenuPanelOpen = False }, Cmd.none )

        UserClickedReadmeButton packageName ->
            case Dict.get packageName model.readmes of
                Just readme ->
                    ( { model | readmes = Dict.update packageName (Maybe.map <| always { readme | isOpen = not readme.isOpen }) model.readmes }, Cmd.none )

                Nothing ->
                    ( model, getReadme packageName )

        UserResizedWindow width height ->
            ( { model | windowSize = { height = height, width = width } }, Cmd.none )

        UserSelectedSubcat subcat ->
            ( { model | selectedPkgSubcat = subcat, isMenuPanelOpen = False }, Cmd.batch [ resetViewport, resetEntryListViewPort ] )

        UserSelectedToolCat cat ->
            ( { model | selectedToolCat = cat, isMenuPanelOpen = False }, Cmd.batch [ resetViewport, resetEntryListViewPort ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize UserResizedWindow


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


isNarrow : Size -> Bool
isNarrow windowSize =
    windowSize.width < 700


menuIcon : List (Attribute msg) -> Element msg
menuIcon attrs =
    textColumn ([ width <| px 30, alignRight, spacing -17, headingTypeface, Font.size 20, Font.color green ] ++ attrs)
        [ paragraph [] [ text "—" ]
        , paragraph [] [ text "—" ]
        , paragraph [] [ text "—" ]
        ]


navBar : Model -> Element Msg
navBar model =
    row
        [ width fill
        , height <| px 70
        , padding 10
        , Background.color panelBgColor
        , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        , Border.color orange
        , headingTypeface
        ]
    <|
        List.concat
            [ [ link [ centerY, height <| px 50 ]
                    { url = "https://korban.net/elm/catalog"
                    , label = image [ width (px 46), height (px 50) ] { src = "https://korban.net/img/logo.png", description = "Korban.net" }
                    }
              , el [ padding 10, centerY, Font.color darkCharcoal, Font.size 26 ] <| text "Elm Catalog"
              ]
            , if isNarrow model.windowSize then
                [ menuIcon [ onClick UserClickedMenuIcon ] ]

              else
                [ el
                    [ paddingEach { left = 20, top = 7, right = 0, bottom = 0 }
                    , centerY
                    , Font.color lightCharcoal
                    , Font.size 16
                    ]
                  <|
                    text <|
                        String.fromInt model.packageCount
                            ++ " Elm 0.19 packages, "
                            ++ String.fromInt model.toolCount
                            ++ " Elm tools"
                , link [ centerY, alignRight, Font.color blue, Font.underline ] { url = "https://korban.net/elm/book", label = text "Practical Elm book" }
                ]
            ]


navigationMenuPanel : Model -> Element Msg
navigationMenuPanel model =
    row [ width fill, height fill ]
        [ el
            [ width fill
            , height fill
            , onClick UserClickedOutsideMenuPanel
            , Background.color <| rgba255 11 79 108 0.4
            ]
            none
        , el [ width <| px 2, height fill, Background.color grey ] none
        , categoryList model
        ]


packageCard : Readmes -> Package -> Element Msg
packageCard readmes package =
    let
        readme =
            Maybe.withDefault { isOpen = False, text = "" } <| Dict.get package.name readmes
    in
    column
        [ width <| maximum 800 fill
        , padding 10
        , spacingXY 0 10
        , Border.width 2
        , Border.rounded 5
        , Border.color lightGrey
        ]
        [ el [ Font.size 20, Font.color blue, headingTypeface ] <|
            link [] { url = "https://package.elm-lang.org/packages/" ++ package.name ++ "/latest/", label = text package.name }
        , el [ height <| px 1, width fill, Background.color lightGreen ] none
        , paragraph [ paddingXY 0 5, Font.size 16 ] [ text package.summary ]
        , wrappedRow [ Font.size 14, Font.color lightCharcoal ]
            [ link [ Font.color lightBlue ] { url = "https://github.com/" ++ package.name, label = text "Source" }
            , text " • "
            , link [ Font.color lightBlue ] { url = "https://elm-greenwood.com/?" ++ String.replace "/" "=" package.name, label = text "Releases" }
            , text " • "
            , text <| "Latest: " ++ (Maybe.withDefault "n/a" <| List.last package.versions)
            , text " • "
            , link [ Font.color lightBlue ] { url = "https://opensource.org/licenses/" ++ package.license, label = text package.license }
            , text " • "
            , el [ Font.color lightBlue, onClick <| UserClickedReadmeButton package.name, pointer ] <|
                text
                    ("README "
                        ++ (if readme.isOpen then
                                "▲"

                            else
                                "▽"
                           )
                    )
            ]
        , if readme.isOpen then
            markdown readme.text

          else
            none
        ]


toolCard : Readmes -> Tool -> Element Msg
toolCard readmes tool =
    let
        readme =
            Maybe.withDefault { isOpen = False, text = "" } <| Dict.get tool.githubName readmes

        packageLabels =
            [ ( "npmjs.com", "NPM package" )
            , ( "hackage.haskell.org", "Hackage package" )
            , ( "atom.io/packages", "Atom package" )
            , ( "marketplace.visualstudio.com", "VS Code package" )
            , ( "packagecontrol.io", "Sublime Text package" )
            , ( "addons.mozilla.org", "Firefox addon" )
            , ( "plugins.jetbrains.com", "JetBrains plugin" )
            ]

        packageLabel url =
            case List.find (\( substr, _ ) -> String.contains substr url) packageLabels of
                Nothing ->
                    "UNKOWN PACKAGE"

                Just ( _, label ) ->
                    label

        packageEl =
            case tool.packageUrl of
                Just url ->
                    row []
                        [ text " • "
                        , link [ Font.color lightBlue ] { url = url, label = text <| packageLabel url }
                        ]

                Nothing ->
                    none

        githubUrl =
            "https://github.com/" ++ tool.githubName

        mainUrl =
            Maybe.or tool.url tool.packageUrl
                |> Maybe.withDefault githubUrl
    in
    column
        [ width <| maximum 800 fill
        , padding 10
        , spacingXY 0 10
        , Border.width 2
        , Border.rounded 5
        , Border.color lightGrey
        ]
        [ el [ Font.size 20, Font.color blue, headingTypeface ] <|
            link [] { url = mainUrl, label = text tool.name }
        , el [ height <| px 1, width fill, Background.color lightGreen ] none
        , paragraph [ paddingXY 0 5, Font.size 16 ] <| [ text tool.summary ]
        , row [ Font.size 14, Font.color lightCharcoal ]
            [ link [ Font.color lightBlue ] { url = githubUrl, label = text "Source" }
            , packageEl
            , text " • "
            , el [ Font.color lightBlue, onClick <| UserClickedReadmeButton tool.githubName, pointer ] <|
                text
                    ("README "
                        ++ (if readme.isOpen then
                                "▲"

                            else
                                "▽"
                           )
                    )
            ]
        , if readme.isOpen then
            markdown readme.text

          else
            none
        ]


pkgCategoryList : Model -> Element Msg
pkgCategoryList model =
    let
        catEls ( cat, subcats ) =
            column [ spacingXY 0 10, Font.size 18 ]
                ((el
                    [ paddingEach { sides | top = 10 }
                    , Font.size 22
                    , Font.color darkCharcoal
                    , headingTypeface
                    ]
                  <|
                    text <|
                        humanisePkgCat cat
                 )
                    :: List.map subcatEl (subcats |> List.sortBy humanisePkgSubcat)
                )

        subcatEl subcat =
            if subcat == model.selectedPkgSubcat then
                el [ Font.color green ] <| text <| humanisePkgSubcat subcat

            else
                el [ Font.color blue, pointer, onClick (UserSelectedSubcat subcat) ] <| text <| humanisePkgSubcat subcat
    in
    Dict.toList model.pkgCategories
        |> List.filter (\( cat, _ ) -> cat /= "exclude")
        |> List.map catEls
        |> column
            [ width fill
            , height fill
            , spacingXY 0 10
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            ]


toolCategoryList : Model -> Element Msg
toolCategoryList model =
    let
        catEl cat =
            if cat == model.selectedToolCat then
                el [ Font.size 18, Font.color green ] <| text <| humaniseToolCat cat

            else
                el [ Font.size 18, Font.color blue, pointer, onClick (UserSelectedToolCat cat) ] <| text <| humaniseToolCat cat
    in
    Dict.keys model.tools
        |> List.map catEl
        |> column
            [ width fill
            , height fill
            , spacingXY 0 10
            , paddingEach { sides | top = 20, bottom = 100 }
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            ]


categoryList : Model -> Element Msg
categoryList model =
    let
        tabEl isSelected url label =
            let
                commonAttrs =
                    [ centerX, centerY, padding 5 ]
            in
            el
                [ width <| fillPortion 1
                , Background.color white
                , Border.widthEach
                    (if isSelected then
                        { top = 2, bottom = 0, left = 2, right = 2 }

                     else
                        { bottom = 2, top = 0, left = 0, right = 0 }
                    )
                , Border.color lightGrey
                ]
            <|
                if isSelected then
                    el (commonAttrs ++ [ Font.color green ]) <|
                        text label

                else
                    link (commonAttrs ++ [ Font.color blue ])
                        { url =
                            if String.isEmpty model.urlPrefix then
                                url

                            else
                                "/" ++ model.urlPrefix ++ url
                        , label = text label
                        }

        tabEls =
            case model.route of
                PackageRoute _ ->
                    [ tabEl True "/packages" "Packages", tabEl False "/tools" "Tools" ]

                ToolRoute _ ->
                    [ tabEl False "/packages" "Packages", tabEl True "/tools" "Tools" ]
    in
    column
        [ htmlAttribute <| Attr.style "flex-shrink" "1"
        , width <| maximum 400 <| minimum 250 <| fillPortion 4
        , height fill
        , if isNarrow model.windowSize then
            Background.color white

          else
            scrollbarY
        , Background.color white
        , Border.widthEach { right = 2, left = 0, bottom = 0, top = 0 }
        , Border.color lightGrey
        ]
        [ row
            [ width fill
            , height <| px 37
            , paddingEach { top = 5, bottom = 0, left = 0, right = 0 }
            ]
            ([ el [ width <| px 5, alignBottom, Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }, Border.color lightGrey ] none ]
                ++ tabEls
                ++ [ el [ width <| px 5, alignBottom, Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }, Border.color lightGrey ] none
                   ]
            )
        , el
            [ paddingEach { sides | left = 10, right = 10, bottom = 20 } ]
          <|
            case model.route of
                PackageRoute _ ->
                    pkgCategoryList model

                ToolRoute _ ->
                    toolCategoryList model
        ]


content : Model -> Element Msg
content model =
    let
        packageEls =
            model.packages
                |> Dict.get model.selectedPkgSubcat
                |> Maybe.withDefault []
                |> List.map (packageCard model.readmes)
                |> List.take (if_ model.isMenuPanelOpen 3 10000)

        toolEls =
            model.tools
                |> Dict.get model.selectedToolCat
                |> Maybe.withDefault []
                |> List.map (toolCard model.readmes)
                |> List.take (if_ model.isMenuPanelOpen 3 10000)

        heading =
            paragraph []
                [ text <|
                    case model.route of
                        PackageRoute _ ->
                            humanisePkgCat (tagCategory model.selectedPkgSubcat) ++ ": " ++ humanisePkgSubcat model.selectedPkgSubcat

                        ToolRoute _ ->
                            humaniseToolCat model.selectedToolCat
                ]
    in
    row
        [ width fill
        , height fill
        , spacingXY 20 0
        , htmlAttribute <| Attr.style "flex-shrink" "1"
        ]
        [ if isNarrow model.windowSize then
            none

          else
            categoryList model
        , column
            [ width <| fillPortion 12
            , height fill
            , paddingXY (if_ (isNarrow model.windowSize) 5 0) 20
            , spacingXY 0 10
            , alignTop
            , if isNarrow model.windowSize then
                attrNone

              else
                scrollbarY
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            , htmlAttribute <| Attr.id "entryList"
            ]
            ([ heading ]
                ++ (case model.route of
                        PackageRoute _ ->
                            packageEls

                        ToolRoute _ ->
                            toolEls
                   )
                ++ [ el [ height <| px 30 ] none
                   , paragraph [ Font.size 16 ]
                        [ text "Found a mistake or a missing entry?"
                        , text " Drop me a line "
                        , link [ Font.color blue ] { url = "https://korban.net/elm/contact", label = text "by email" }
                        , text " or tweet "
                        , link [ Font.color blue ] { url = "https://twitter.com/alexkorban", label = text "@alexkorban" }
                        , text "."
                        ]
                   , el [ height <| px 30 ] none
                   , if_ model.isMenuPanelOpen none bookFooter
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
    el
        [ width <| maximum 800 fill
        , Font.size 16
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        , Border.color lightGrey
        ]
    <|
        Element.html <|
            Markdown.toHtmlWith mdOptions
                [ Attr.class "markdown" ]
                s


bookFooter : Element Msg
bookFooter =
    let
        p =
            paragraph [ Font.size 16, spacing 10 ]
    in
    row
        [ paddingXY 20 20
        , spacing 30
        , Border.widthEach { top = 2, bottom = 0, left = 0, right = 0 }
        , Border.color <| orange
        , width <| maximum 800 fill
        , baseTypeface
        ]
        [ textColumn
            [ spacing 30, alignTop, width (fillPortion 3) ]
            [ paragraph [ Font.size 24, headingTypeface, spacingXY 10 10 ]
                [ text "Are you building non-trivial apps in Elm? This book will help you." ]
            , p
                [ text "My book "
                , link [ Font.color blue ] { url = "https://korban.net/elm/book", label = text "Practical Elm" }
                , text " skips the basics and gets right into explaining how to do practical stuff."
                ]
            , p
                [ text "Things like building out the UI, communicating with servers, parsing JSON, structuring the application as it grows, testing, and so on. No handholding – the focus is on giving you more substance." ]
            , p [ text "It's up to date with Elm 0.19." ]
            , p [ text "Pop in your email to get a sample chapter." ]
            , paragraph [ Font.size 14 ] [ text "(You will also get notifications of new posts along with other mailing list only freebies.)" ]
            , el [ height <| px 30 ] none
            ]
        , column
            [ paddingXY 20 10
            , spacing 5
            , height fill
            , width (fillPortion 2)
            ]
            [ el [ width (px 208), height <| px 267, centerX, Border.width 2, Border.color grey ] <|
                el [ width (px 204), height <| px 263, Border.width 2, Border.color darkCharcoal ] <|
                    image [ width (px 200) ] { src = "https://korban.net/img/practical-elm-cover.jpg", description = "Book cover" }
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
                                    , span [ Attr.style "font-weight" "bold" ]
                                        [ Html.text "Send me a sample chapter" ]
                                    ]
                                ]
                            ]
                        , node "style"
                            []
                            [ Html.text ".formkit-form[data-uid=\"8b09e227e0\"] *{font-family:\"Helvetica Neue\",Helvetica,Arial,Verdana,sans-serif;box-sizing:border-box;}.formkit-form[data-uid=\"8b09e227e0\"]{-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;}.formkit-form[data-uid=\"8b09e227e0\"] legend{border:none;font-size:inherit;margin-bottom:10px;padding:0;position:relative;display:table;}.formkit-form[data-uid=\"8b09e227e0\"] fieldset{border:0;padding:0.01em 0 0 0;margin:0;min-width:0;}.formkit-form[data-uid=\"8b09e227e0\"] body:not(:-moz-handler-blocked) fieldset{display:table-cell;}.formkit-form[data-uid=\"8b09e227e0\"] h1,.formkit-form[data-uid=\"8b09e227e0\"] h2,.formkit-form[data-uid=\"8b09e227e0\"] h3,.formkit-form[data-uid=\"8b09e227e0\"] h4,.formkit-form[data-uid=\"8b09e227e0\"] h5,.formkit-form[data-uid=\"8b09e227e0\"] h6{color:inherit;font-size:inherit;font-weight:inherit;}.formkit-form[data-uid=\"8b09e227e0\"] p{color:inherit;font-size:inherit;font-weight:inherit;}.formkit-form[data-uid=\"8b09e227e0\"][data-format=\"modal\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"][data-format=\"slide in\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-select,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-checkboxes{width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit{border:0;border-radius:5px;color:#ffffff;cursor:pointer;display:inline-block;text-align:center;font-size:15px;font-weight:500;cursor:pointer;margin-bottom:15px;overflow:hidden;padding:0;position:relative;vertical-align:middle;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:hover,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:hover,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:focus,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:focus{outline:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:hover > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:hover > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button:focus > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit:focus > span{background-color:rgba(0,0,0,0.1);}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-button > span,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit > span{display:block;-webkit-transition:all 300ms ease-in-out;transition:all 300ms ease-in-out;padding:12px 24px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input{background:#ffffff;font-size:15px;padding:12px;border:1px solid #e3e3e3;-webkit-flex:1 0 auto;-ms-flex:1 0 auto;flex:1 0 auto;line-height:1.4;margin:0;-webkit-transition:border-color ease-out 300ms;transition:border-color ease-out 300ms;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input:focus{outline:none;border-color:#1677be;-webkit-transition:border-color ease 300ms;transition:border-color ease 300ms;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::-webkit-input-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::-moz-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input:-ms-input-placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-input::placeholder{color:inherit;opacity:0.8;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"]{position:relative;display:inline-block;width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"]::before{content:\"\";top:calc(50% - 2.5px);right:10px;position:absolute;pointer-events:none;border-color:#4f4f4f transparent transparent transparent;border-style:solid;border-width:6px 6px 0 6px;height:0;width:0;z-index:999;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"] select{height:auto;width:100%;cursor:pointer;color:#333333;line-height:1.4;margin-bottom:0;padding:0 6px;-webkit-appearance:none;-moz-appearance:none;appearance:none;font-size:15px;padding:12px;padding-right:25px;border:1px solid #e3e3e3;background:#ffffff;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"dropdown\"] select:focus{outline:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"]{text-align:left;margin:0;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"]{margin-bottom:10px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] *{cursor:pointer;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"]:last-of-type{margin-bottom:0;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"] + label::after{content:none;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]:checked + label::after{border-color:#ffffff;content:\"\";}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] input[type=\"checkbox\"]:checked + label::before{background:#10bf7a;border-color:#10bf7a;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label{position:relative;display:inline-block;padding-left:28px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::before,.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::after{position:absolute;content:\"\";display:inline-block;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::before{height:16px;width:16px;border:1px solid #e3e3e3;background:#ffffff;left:0px;top:3px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-group=\"checkboxes\"] [data-group=\"checkbox\"] label::after{height:4px;width:8px;border-left:2px solid #4d4d4d;border-bottom:2px solid #4d4d4d;-webkit-transform:rotate(-45deg);-ms-transform:rotate(-45deg);transform:rotate(-45deg);left:4px;top:8px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert{background:#f9fafb;border:1px solid #e3e3e3;border-radius:5px;-webkit-flex:1 0 auto;-ms-flex:1 0 auto;flex:1 0 auto;list-style:none;margin:25px auto;padding:12px;text-align:center;width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert:empty{display:none;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert-success{background:#d3fbeb;border-color:#10bf7a;color:#0c905c;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-alert-error{background:#fde8e2;border-color:#f2643b;color:#ea4110;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner{display:-webkit-box;display:-webkit-flex;display:-ms-flexbox;display:flex;height:0px;width:0px;margin:0 auto;position:absolute;top:0;left:0;right:0;width:0px;overflow:hidden;text-align:center;-webkit-transition:all 300ms ease-in-out;transition:all 300ms ease-in-out;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div{margin:auto;width:12px;height:12px;background-color:#fff;opacity:0.3;border-radius:100%;display:inline-block;-webkit-animation:formkit-bouncedelay-formkit-form-data-uid-8b09e227e0- 1.4s infinite ease-in-out both;animation:formkit-bouncedelay-formkit-form-data-uid-8b09e227e0- 1.4s infinite ease-in-out both;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div:nth-child(1){-webkit-animation-delay:-0.32s;animation-delay:-0.32s;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-spinner > div:nth-child(2){-webkit-animation-delay:-0.16s;animation-delay:-0.16s;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit[data-active] .formkit-spinner{opacity:1;height:100%;width:50px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit[data-active] .formkit-spinner ~ span{opacity:0;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-powered-by[data-active=\"false\"]{opacity:0.35;}@-webkit-keyframes formkit-bouncedelay-formkit-form-data-uid-8b09e227e0-{0%,80%,100%{-webkit-transform:scale(0);-ms-transform:scale(0);transform:scale(0);}40%{-webkit-transform:scale(1);-ms-transform:scale(1);transform:scale(1);}}@keyframes formkit-bouncedelay-formkit-form-data-uid-8b09e227e0-{0%,80%,100%{-webkit-transform:scale(0);-ms-transform:scale(0);transform:scale(0);}40%{-webkit-transform:scale(1);-ms-transform:scale(1);transform:scale(1);}} .formkit-form[data-uid=\"8b09e227e0\"]{max-width:700px;}.formkit-form[data-uid=\"8b09e227e0\"] [data-style=\"clean\"]{width:100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-fields{display:-webkit-box;display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-wrap:wrap;-ms-flex-wrap:wrap;flex-wrap:wrap;margin:0 auto;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit{margin:0 0 15px 0;-webkit-flex:1 0 100%;-ms-flex:1 0 100%;flex:1 0 100%;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-powered-by{color:#7d7d7d;display:block;font-size:12px;margin:0;text-align:center;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] [data-style=\"clean\"],.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] [data-style=\"clean\"]{padding:10px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"],.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"]{margin-left:-5px;margin-right:-5px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit{margin:0 5px 15px 5px;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-field,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-field{-webkit-flex:100 1 auto;-ms-flex:100 1 auto;flex:100 1 auto;}.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"700\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit,.formkit-form[data-uid=\"8b09e227e0\"][min-width~=\"800\"] .formkit-fields[data-stacked=\"false\"] .formkit-submit{-webkit-flex:1 1 auto;-ms-flex:1 1 auto;flex:1 1 auto;} .formkit-form[data-uid=\"8b09e227e0\"] input{border:2px solid #e0e0e0;font-family:\"Open Sans\",Helvetica,\"Arial\",sans-serif;font-size:18px;}.formkit-form[data-uid=\"8b09e227e0\"] .formkit-submit span{box-sizing:border-box;font-weight:bold;font-family:\"Cairo\",sans-serif;border:2px solid #3f8438;}" ]
                        ]
            ]
        ]


baseTypeface : Element.Attribute msg
baseTypeface =
    Font.family [ Font.typeface "Open Sans", Font.typeface "Helvetica", Font.typeface "Arial", Font.serif ]


headingTypeface : Element.Attribute msg
headingTypeface =
    Font.family [ Font.typeface "Cairo", Font.typeface "Helvetica", Font.sansSerif ]


view : Model -> Html Msg
view model =
    let
        attrs =
            [ width fill
            , height fill
            , htmlAttribute <| Attr.style "flex-shrink" "1"
            ]

        attrsWithMenu =
            (if model.isMenuPanelOpen then
                inFront <| navigationMenuPanel model

             else
                attrNone
            )
                :: attrs
    in
    layout
        [ height fill
        , baseTypeface
        , htmlAttribute <| Attr.style "flex-shrink" "1"
        ]
    <|
        column attrsWithMenu
            [ navBar model
            , content model
            ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = \model -> { title = "Elm Catalog", body = [ view model ] }
        , subscriptions = subscriptions
        , onUrlRequest = UserClickedLink
        , onUrlChange = RuntimeChangedUrl
        }
