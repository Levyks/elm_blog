module Misc exposing (apiUrl, formInput, getHttpErrorMessage)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Styled.Events exposing (onInput)
import Http.Detailed
import Json.Decode as Decode


apiUrl : String
apiUrl =
    "http://localhost:3000"


type alias FormInputParams msg =
    { id : String
    , label : String
    , type_ : String
    , value : String
    , onInput : String -> msg
    , required : Bool
    , disabled : Bool
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
            , disabled params.disabled
            , Html.Styled.Attributes.required params.required
            , onInput params.onInput
            ]
            []
        ]


type alias BodyWithMessage =
    { message : String
    }


bodyWithMessageDecoder : Decode.Decoder BodyWithMessage
bodyWithMessageDecoder =
    Decode.map BodyWithMessage
        (Decode.field "message" Decode.string)


getMessageFromBody : String -> String
getMessageFromBody body =
    case Decode.decodeString bodyWithMessageDecoder body of
        Ok error ->
            error.message

        Err _ ->
            "Unknown error"


getHttpErrorMessage : Http.Detailed.Error String -> String
getHttpErrorMessage error =
    case error of
        Http.Detailed.BadUrl message ->
            message

        Http.Detailed.Timeout ->
            "The server is taking too long to respond."

        Http.Detailed.NetworkError ->
            "There was a network error."

        Http.Detailed.BadStatus _ body ->
            getMessageFromBody body

        Http.Detailed.BadBody _ body _ ->
            getMessageFromBody body
