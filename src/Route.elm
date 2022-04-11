module Route exposing (..)

import Pagination exposing (pageParser)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | ListPosts Int
    | Login


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
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
