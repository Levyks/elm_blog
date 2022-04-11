module Layout exposing (mainLayout)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, classList, href, id, type_)
import Misc exposing (AlertParams, getAlertClass)


navbar : Html msg
navbar =
    nav
        [ class "navbar navbar-expand-lg navbar-dark bg-dark"
        ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ] [ text "Navbar" ]
            , button
                [ class "navbar-toggler"
                , type_ "button"
                , attribute "data-bs-toggle" "collapse"
                , attribute "data-bs-target" "#navbarNav"
                , attribute "aria-controls" "navbarNav"
                , attribute "aria-expanded" "false"
                , attribute "aria-label" "Toggle navigation"
                ]
                [ span [ attribute "class" "navbar-toggler-icon" ] [] ]
            , div
                [ class "collapse navbar-collapse"
                , id "navbarNav"
                ]
                [ div [ class "navbar-nav" ]
                    []
                ]
            ]
        ]


mainLayout : Html msg -> List AlertParams -> Html msg
mainLayout inner alerts =
    div
        [ class "d-flex flex-column h-100"
        ]
        [ navbar
        , div [ class "container mt-2" ] (List.map viewAlert alerts)
        , div
            [ class "flex-grow-1" ]
            [ inner ]
        ]


viewAlert : AlertParams -> Html msg
viewAlert alert =
    div
        [ class (String.join " " [ "alert", getAlertClass alert ])
        , classList [ ( "alert-dismissible", alert.dismissable ) ]
        ]
        [ text alert.message
        , if alert.dismissable then
            button
                [ class "btn-close"
                , type_ "button"
                , attribute "data-bs-dismiss" "alert"
                , attribute "aria-label" "Close"
                ]
                []

          else
            text ""
        ]
