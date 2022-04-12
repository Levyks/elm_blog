module Route exposing (..)

import Debug
import Pagination exposing (pageParser)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | ListPosts Int
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


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map ListPosts (top <?> pageParser)
        , map ListPosts (s "posts" <?> pageParser)
        , map Login (s "login")
        ]
