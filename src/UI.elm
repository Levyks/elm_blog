module UI exposing (viewDelayNotice, viewFullPageSpinner, viewIcon, viewSearchField, viewSpinner)

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


viewFullPageSpinner : Float -> Float -> Bool -> Html msg
viewFullPageSpinner size strokeWidth showDelayNotice =
    div [ class "flex-grow-1 d-flex flex-column justify-content-center align-items-center" ]
        (viewSpinner 8 1.5
            :: (if showDelayNotice then
                    [ viewDelayNotice ]

                else
                    []
               )
        )


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
