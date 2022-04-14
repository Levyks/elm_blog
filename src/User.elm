module User exposing (BasicUser, UserId, basicUserDecoder, getUrl, idParser, idToString)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser, custom)


type UserId
    = UserId Int


idDecoder : Decoder UserId
idDecoder =
    Decode.map UserId int


idToString : UserId -> String
idToString (UserId id) =
    String.fromInt id


idParser : Parser (UserId -> a) a
idParser =
    custom "USERID" <|
        \userId ->
            Maybe.map UserId (String.toInt userId)


type alias BasicUser =
    { id : UserId
    , name : String
    }


getUrl : UserId -> String
getUrl (UserId id) =
    "users/" ++ String.fromInt id


basicUserDecoder : Decoder BasicUser
basicUserDecoder =
    Decode.succeed BasicUser
        |> required "id" idDecoder
        |> required "name" string
