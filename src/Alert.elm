module Alert exposing (AlertParams, AlertType(..), view)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, classList, type_)


type AlertType
    = Success
    | Info
    | Warning
    | Danger


type alias AlertParams =
    { type_ : AlertType
    , message : String
    , dismissable : Bool
    }


getAlertClass : AlertParams -> String
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


view : AlertParams -> Html msg
view alert =
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
