module Route exposing (..)

import Pagination exposing (pageParser)
import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = NotFound
    | ListPosts Int String
    | Login


removeBase : String -> Url -> Url
removeBase base url =
    let
        path =
            String.right (String.length url.path - String.length base + 1) url.path
    in
    Url url.protocol url.host url.port_ path url.query url.fragment


parseUrl : String -> Url -> Route
parseUrl baseUrl url =
    case parse matchRoute (removeBase baseUrl url) of
        Just route ->
            route

        Nothing ->
            NotFound


searchParser : Query.Parser String
searchParser =
    Query.map (Maybe.withDefault "") (Query.string "q")


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map ListPosts (top <?> pageParser <?> searchParser)
        , map ListPosts (s "posts" <?> pageParser <?> searchParser)
        , map Login (s "login")
        ]
