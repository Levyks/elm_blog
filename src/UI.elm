module UI exposing (viewSpinner)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)


viewSpinner : Float -> Float -> Html msg
viewSpinner size strokeWidth =
    div
        [ class "spinner-border me-2"
        , css
            [ width (rem size)
            , height (rem size)
            , fontSize (rem strokeWidth)
            ]
        ]
        [ span
            [ class "visually-hidden"
            ]
            [ text "Loading..." ]
        ]
