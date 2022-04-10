module Misc exposing (formInput)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, for, id, type_, value)
import Html.Styled.Events exposing (onInput)


type alias FormInputParams msg =
    { id : String
    , label : String
    , type_ : String
    , value : String
    , onInput : String -> msg
    , required : Bool
    }


formInput : FormInputParams msg -> Html msg
formInput params =
    div [ class "mb-3" ]
        [ label [ for params.id ] [ text params.label ]
        , input
            [ class "form-control"
            , id params.id
            , type_ params.type_
            , value params.value
            , Html.Styled.Attributes.required params.required
            , onInput params.onInput
            ]
            []
        ]
