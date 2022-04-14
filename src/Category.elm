module Category exposing (BasicCategory, Category, CategoryId, basicCategoryDecoder, categoryDecoder, getUrl, idParser, idToString)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Url.Parser exposing (Parser, custom)


type CategoryId
    = CategoryId Int


idDecoder : Decoder CategoryId
idDecoder =
    Decode.map CategoryId int


idToString : CategoryId -> String
idToString (CategoryId id) =
    String.fromInt id


idParser : Parser (CategoryId -> a) a
idParser =
    custom "CATEGORYID" <|
        \categoryId ->
            Maybe.map CategoryId (String.toInt categoryId)


type alias BasicCategory =
    { id : CategoryId
    , name : String
    }


type alias Category =
    { id : CategoryId
    , name : String
    , postCount : Int
    }


getUrl : CategoryId -> String
getUrl (CategoryId id) =
    "categories/" ++ String.fromInt id


basicCategoryDecoder : Decoder BasicCategory
basicCategoryDecoder =
    Decode.succeed BasicCategory
        |> required "id" idDecoder
        |> required "name" string


categoryDecoder : Decoder Category
categoryDecoder =
    Decode.succeed Category
        |> required "id" idDecoder
        |> required "title" string
        |> required "postCount" int
