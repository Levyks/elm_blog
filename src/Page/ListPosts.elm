module Page.ListPosts exposing (Model, Msg, init, title, update, view)

import Css exposing (..)
import DateTime
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, href)
import HttpHelper exposing (WebDetailedData)
import Layout exposing (mainLayout)
import Pagination exposing (Pagination, fetchPage, viewPaginationRemoteData)
import Post exposing (BasicPost, basicPostDecoder)
import RemoteData
import Util exposing (delay)


type alias Model =
    { posts : WebDetailedData (Pagination BasicPost)
    , page : Int
    , showDelayNotice : Bool
    , search : String
    }


init : Int -> String -> ( Model, Cmd Msg )
init page search =
    ( { posts = RemoteData.Loading
      , page = page
      , showDelayNotice = False
      , search = search
      }
    , fetchPosts page search
    )


title : String
title =
    "Posts | Elm Blog"


view : Model -> Html Msg
view model =
    mainLayout
        (viewPaginationRemoteData
            { title = "Posts"
            , data = model.posts
            , search = model.search
            , onSearchInput = SearchInput
            , view = viewPost
            }
        )
        []


viewPost : BasicPost -> Html Msg
viewPost post =
    a
        [ href (Post.getUrl post.id)
        ]
        [ div
            [ class "card mb-3"
            ]
            [ div [ class "card-body" ]
                [ h5 [ class "card-subtitle mb-2 text-info" ] [ text post.category ]
                , h2 [ class "card-title" ] [ text post.title ]
                , h6 [ class "card-subtitle mb-3 text-muted" ]
                    [ DateTime.viewTimeAgo post.createdAt
                    , text (" | by " ++ post.author)
                    ]
                , p [ class "card-text" ] [ text post.contentShort ]
                ]
            ]
        ]


type Msg
    = FetchPosts
    | PostsReceived (WebDetailedData (Pagination BasicPost))
    | SearchInput String
    | DelayedSearchInput String


fetchPosts : Int -> String -> Cmd Msg
fetchPosts page search =
    fetchPage
        { endpoint = "posts"
        , page = page
        , search = search
        , size = 5
        , sort = ( "createdAt", Pagination.DESC )
        , decoder = basicPostDecoder
        , msg = PostsReceived
        }


scheduleDelayedSearchInput : String -> Cmd Msg
scheduleDelayedSearchInput search =
    delay 500 (DelayedSearchInput search)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( model, fetchPosts model.page model.search )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        SearchInput search ->
            ( { model | search = search }, scheduleDelayedSearchInput search )

        DelayedSearchInput debouncedSearch ->
            if debouncedSearch == model.search then
                ( { model | search = debouncedSearch }
                , fetchPosts 1 model.search
                )

            else
                ( model, Cmd.none )
