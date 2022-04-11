module Post exposing (BasicPost, Post, PostId, basicPostDecoder, getUrl, idToString, postDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import Time


type PostId
    = PostId Int


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


type alias BasicPost =
    { id : PostId
    , title : String
    , contentShort : String
    , createdAt : Time.Posix
    , author : String
    , category : String
    }


type alias Post =
    { id : PostId
    , title : String
    , content : String
    }


getUrl : PostId -> String
getUrl (PostId id) =
    "posts/" ++ String.fromInt id


basicPostDecoder : Decoder BasicPost
basicPostDecoder =
    Decode.succeed BasicPost
        |> required "id" idDecoder
        |> required "title" string
        |> required "contentShort" string
        |> required "createdAt" datetime
        |> required "author" string
        |> required "category" string


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "content" string
