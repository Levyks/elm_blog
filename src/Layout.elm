module Layout exposing (mainLayout, maybeRender)

import Alert exposing (AlertParams)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, href, id, type_)


maybeRender : Bool -> Html msg -> Html msg
maybeRender render inner =
    if render then
        inner

    else
        text ""


navbar : Html msg
navbar =
    nav
        [ class "navbar navbar-expand-lg navbar-dark bg-dark"
        ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "/" ] [ text "Elm Blog" ]
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


mainLayout : Html msg -> List (AlertParams msg) -> Html msg
mainLayout inner alerts =
    div
        [ class "d-flex flex-column h-100"
        ]
        [ navbar
        , div [ class "container mt-2" ] (List.map Alert.view alerts)
        , div
            [ class "flex-grow-1" ]
            [ inner ]
        ]
