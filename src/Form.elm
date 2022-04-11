module Form exposing (input)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, disabled, for, id, required, type_, value)
import Html.Styled.Events exposing (onInput)


type alias InputParams msg =
    { id : String
    , label : String
    , type_ : String
    , value : String
    , onInput : String -> msg
    , required : Bool
    , disabled : Bool
    }


input : InputParams msg -> Html msg
input params =
    div [ class "mb-3" ]
        [ label [ for params.id ] [ text params.label ]
        , Html.Styled.input
            [ class "form-control"
            , id params.id
            , type_ params.type_
            , value params.value
            , disabled params.disabled
            , required params.required
            , onInput params.onInput
            ]
            []
        ]
