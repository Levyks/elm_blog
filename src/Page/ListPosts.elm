module Page.ListPosts exposing (Model, Msg, init, title, update, view)

import Css exposing (..)
import DateTime
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href)
import Http
import Http.Detailed
import HttpHelper exposing (WebDetailedData, apiEndpoint, getHttpErrorMessage, webDataFromResultDetailed)
import Layout exposing (mainLayout)
import Pagination exposing (Pagination, paginationDecoder)
import Post exposing (BasicPost, basicPostDecoder)
import RemoteData
import UI exposing (viewSpinner)
import Util exposing (delay)


type alias Model =
    { posts : WebDetailedData (Pagination BasicPost)
    , page : Int
    , showDelayNotice : Bool
    }


init : Int -> ( Model, Cmd Msg )
init page =
    ( { posts = RemoteData.Loading
      , page = page
      , showDelayNotice = False
      }
    , Cmd.batch [ fetchPosts page, scheduleDelayNotice ]
    )


title : String
title =
    "Posts | Elm Blog"


view : Model -> Html Msg
view model =
    mainLayout
        (div
            [ class "container d-flex flex-column h-100"
            ]
            [ h1 [ class "text-center" ] [ text "Posts" ]
            , viewPosts model.posts model.showDelayNotice
            ]
        )
        []


viewPosts : WebDetailedData (Pagination BasicPost) -> Bool -> Html Msg
viewPosts posts showDelayNotice =
    case posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "flex-grow-1 d-flex flex-column justify-content-center align-items-center" ]
                [ viewSpinner 8 1.5
                , span [ class "mt-5" ]
                    [ if showDelayNotice then
                        text "Heroku can take a few seconds on a cold start. Please wait."

                      else
                        text ""
                    ]
                ]

        RemoteData.Failure err ->
            div [] [ text ("Error: " ++ getHttpErrorMessage err) ]

        RemoteData.Success postsPag ->
            div
                []
                [ div
                    [ css
                        [ width (px 700)
                        , maxWidth (calc (pct 100) minus (px 20))
                        , margin2 (px 0) auto
                        ]
                    ]
                    (List.map viewPost postsPag.content)
                , Pagination.buttons postsPag
                ]


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


fetchPosts : Int -> Cmd Msg
fetchPosts page =
    Http.get
        { url = apiEndpoint ("posts?size=5&page=" ++ String.fromInt (page - 1))
        , expect =
            Http.Detailed.expectJson (webDataFromResultDetailed >> PostsReceived) (paginationDecoder basicPostDecoder)
        }


scheduleDelayNotice : Cmd Msg
scheduleDelayNotice =
    delay 3000 DelayNoticeReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPost ->
            ( model, Cmd.batch [ fetchPosts model.page, scheduleDelayNotice ] )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        DelayNoticeReceived ->
            case model.posts of
                RemoteData.Loading ->
                    ( { model | showDelayNotice = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
