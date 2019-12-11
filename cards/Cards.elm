module Cards exposing (main)

import Element exposing (..)
import Main as App
import UiCards exposing (card, cardError, deck, show)



-- This is your application's main module


package =
    { name = "alexkorban/uicards"
    , summary = "UICards is a UI prototyping tool for Elm."
    , license = "GPL 3.0"
    , versions = [ "1.0.0", "1.0.1", "1.0.2" ]
    , tags = [ "dev/prototyping" ]
    }


initialModel =
    { categories = categorisePackages [ package ]
    }


main =
    show App.update
        [ deck "Wide layout"
            [ card "Layout" initialModel <|
                \model ->
                    App.view model
            , card "Package card" initialModel <|
                \model ->
                    layout [] <| App.packageCard package
            ]
        ]
