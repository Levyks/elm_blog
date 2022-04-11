module HttpHelper exposing (apiUrl, getHttpErrorMessage)

import Http.Detailed
import Json.Decode as Decode


apiUrl : String
apiUrl =
    "http://localhost:3000"


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
