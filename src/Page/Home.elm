module Page.Home exposing (view)

import Html.Styled exposing (..)
import Layout exposing (mainLayout)


view : Html msg
view =
    mainLayout
        (div []
            [ h1 [] [ text "Hello World!" ]
            ]
        )
        []
