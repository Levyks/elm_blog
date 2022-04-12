module HttpHelper exposing (WebDetailedData, apiEndpoint, getHttpErrorMessage, getQueryString, pageParam, searchParam, sortAscParam, sortDescParam, webDataFromResultDetailed)

import Http
import Http.Detailed
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))


apiUrl : String
apiUrl =
    "https://levy-spring-blog.herokuapp.com"


apiEndpoint : String -> String
apiEndpoint path =
    if String.startsWith "/" path then
        apiUrl ++ path

    else
        apiUrl ++ "/" ++ path


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
            "Error with malformed JSON"


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

        Http.Detailed.BadBody _ _ _ ->
            "Malformed JSON response"


type alias WebDetailedData a =
    RemoteData (Http.Detailed.Error String) a


webDataFromResultDetailed : Result (Http.Detailed.Error String) ( Http.Metadata, a ) -> WebDetailedData a
webDataFromResultDetailed result =
    case result of
        Ok ( _, value ) ->
            RemoteData.Success value

        Err error ->
            RemoteData.Failure error


searchParam : String -> ( String, String )
searchParam param =
    if String.isEmpty param then
        ( "", "" )

    else
        ( "q", param )


pageParam : Int -> ( String, String )
pageParam page =
    ( "page", String.fromInt (page - 1) )


sortDescParam : String -> ( String, String )
sortDescParam param =
    ( "sort", param ++ ",DESC" )


sortAscParam : String -> ( String, String )
sortAscParam param =
    ( "sort", param ++ ",ASC" )


isKeyNotEmpty : ( String, String ) -> Bool
isKeyNotEmpty ( key, _ ) =
    not (String.isEmpty key)


mapQueryKeyValue : ( String, String ) -> String
mapQueryKeyValue ( key, value ) =
    key ++ "=" ++ value


getQueryString : List ( String, String ) -> String
getQueryString query =
    if List.isEmpty query then
        ""

    else
        "?" ++ String.join "&" (query |> List.filter isKeyNotEmpty |> List.map mapQueryKeyValue)
