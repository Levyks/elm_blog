module UI exposing (viewDelayNotice, viewIcon, viewSearchField, viewSpinner)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, placeholder, value)
import Html.Styled.Events exposing (onInput)


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


viewDelayNotice : Html msg
viewDelayNotice =
    span [ class "mt-5 appear-after-delay" ]
        [ text "Heroku can take a few seconds on a cold start. Please wait."
        ]


viewIcon : String -> Html msg
viewIcon icon =
    span
        [ class "material-icons user-select-none"
        ]
        [ text icon ]


viewSearchField : (String -> msg) -> String -> Html msg
viewSearchField inputHandler search =
    div
        [ class "input-group"
        , css
            [ maxWidth (rem 20)
            ]
        ]
        [ input
            [ class "form-control"
            , placeholder "Search"
            , onInput inputHandler
            , value search
            ]
            []
        , span [ class "input-group-text" ]
            [ viewIcon "search" ]
        ]
