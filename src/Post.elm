module Post exposing (BasicPost, Post, PostId, basicPostDecoder, getUrl, idParser, idToString, postDecoder)

import Category exposing (BasicCategory, basicCategoryDecoder)
import Json.Decode as Decode exposing (Decoder, bool, int, string)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required)
import Time
import Url.Parser exposing (Parser, custom)
import User exposing (BasicUser, basicUserDecoder)


type PostId
    = PostId Int


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map PostId (String.toInt postId)


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
    , edited : Bool
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , author : BasicUser
    , category : BasicCategory
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
        |> required "edited" bool
        |> required "createdAt" datetime
        |> required "updatedAt" datetime
        |> required "author" basicUserDecoder
        |> required "category" basicCategoryDecoder
