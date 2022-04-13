module Page.ListPosts exposing (Model, Msg, init, title, update, view)

import Css exposing (..)
import DateTime
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href)
import Http
import Http.Detailed
import HttpHelper exposing (SortDirection(..), WebDetailedData, apiEndpoint, getHttpErrorMessage, getQueryString, pageParam, searchParam, sortDescParam, webDataFromResultDetailed)
import Layout exposing (mainLayout)
import Pagination exposing (Pagination, fetchPage, paginationDecoder, viewPageBtns)
import Ports exposing (updateQueryParam)
import Post exposing (BasicPost, basicPostDecoder)
import RemoteData
import UI exposing (viewSearchField, viewSpinner)
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
    , Cmd.batch [ fetchPosts page search, scheduleDelayNotice ]
    )


title : String
title =
    "Posts | Elm Blog"


view : Model -> Html Msg
view model =
    mainLayout
        (div
            [ class "container d-flex flex-column h-100 mt-3"
            ]
            [ h1 [ class "text-center" ] [ text "Posts" ]
            , viewPosts model
            ]
        )
        []


viewPosts : Model -> Html Msg
viewPosts model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "flex-grow-1 d-flex flex-column justify-content-center align-items-center" ]
                [ viewSpinner 8 1.5
                , span [ class "mt-5" ]
                    [ if model.showDelayNotice then
                        text "Heroku can take a few seconds on a cold start. Please wait."

                      else
                        text ""
                    ]
                ]

        RemoteData.Failure err ->
            div [] [ text ("Error: " ++ getHttpErrorMessage err) ]

        RemoteData.Success postsPag ->
            viewPostsSuccess model postsPag


getPageHref : Model -> Int -> String
getPageHref model page =
    getQueryString
        [ pageParam page
        , searchParam model.search
        ]


viewPostsSuccess : Model -> Pagination BasicPost -> Html Msg
viewPostsSuccess model posts =
    div
        []
        [ viewPostsHeader model
        , viewPostsList posts.content
        , viewPageBtns posts (getPageHref model)
        ]


viewPostsHeader : Model -> Html Msg
viewPostsHeader model =
    div
        [ class "d-flex justify-content-end mb-4" ]
        [ viewSearchField SearchInput model.search ]


viewPostsList : List BasicPost -> Html Msg
viewPostsList posts =
    div
        [ css
            [ width (px 700)
            , maxWidth (calc (pct 100) minus (px 20))
            , margin2 (px 0) auto
            ]
        ]
        (List.map viewPost posts)


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
    = FetchPost
    | PostsReceived (WebDetailedData (Pagination BasicPost))
    | DelayNoticeReceived
    | SearchInput String
    | DelayedSearchInput String


fetchPosts : Int -> String -> Cmd Msg
fetchPosts page search =
    fetchPage
        { endpoint = "posts"
        , page = page
        , search = search
        , size = 5
        , sort = ( "createdAt", HttpHelper.DESC )
        , decoder = basicPostDecoder
        , msg = PostsReceived
        }


scheduleDelayNotice : Cmd Msg
scheduleDelayNotice =
    delay 3000 DelayNoticeReceived


searchDebounceMs : Float
searchDebounceMs =
    500


scheduleDelayedSearchInput : String -> Cmd Msg
scheduleDelayedSearchInput search =
    delay searchDebounceMs (DelayedSearchInput search)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPost ->
            ( model, Cmd.batch [ fetchPosts model.page model.search, scheduleDelayNotice ] )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        DelayNoticeReceived ->
            case model.posts of
                RemoteData.Loading ->
                    ( { model | showDelayNotice = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SearchInput search ->
            ( { model | search = search }, scheduleDelayedSearchInput search )

        DelayedSearchInput debouncedSearch ->
            if debouncedSearch == model.search then
                ( { model | search = debouncedSearch }
                , Cmd.batch [ fetchPosts 1 model.search, scheduleDelayNotice ]
                )

            else
                ( model, Cmd.none )
