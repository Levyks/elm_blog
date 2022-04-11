module Alert exposing (AlertParams, AlertType(..), view)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, classList, type_)
import Html.Styled.Events exposing (onClick)


type AlertType
    = Success
    | Info
    | Warning
    | Danger


type alias AlertParams msg =
    { type_ : AlertType
    , message : String
    , dismissable : Bool
    , autoDismissOnClick : Bool
    , onDismiss : Maybe msg
    }


getAlertClass : AlertParams msg -> String
getAlertClass alert =
    case alert.type_ of
        Success ->
            "alert-success"

        Info ->
            "alert-info"

        Warning ->
            "alert-warning"

        Danger ->
            "alert-danger"


view : AlertParams msg -> Html msg
view alert =
    div
        [ class ("alert " ++ getAlertClass alert)
        , classList [ ( "alert-dismissible", alert.dismissable ) ]
        ]
        [ text alert.message
        , if alert.dismissable then
            button
                ([ class "btn-close"
                 , type_ "button"
                 , attribute "aria-label" "Close"
                 ]
                    ++ (case alert.onDismiss of
                            Nothing ->
                                []

                            Just msg ->
                                [ onClick msg ]
                       )
                    ++ (if alert.autoDismissOnClick then
                            [ attribute "data-bs-dismiss" "alert" ]

                        else
                            []
                       )
                )
                []

          else
            text ""
        ]
