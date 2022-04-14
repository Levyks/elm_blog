module Page.ViewPost exposing (Model, Msg, getBasicPostIfTheSame, init, title, update, view)

import Category
import Css exposing (..)
import DateTime
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, href)
import Http
import Http.Detailed
import HttpHelper exposing (WebDetailedData, apiEndpoint, getHttpErrorMessage, webDataFromResultDetailed)
import Layout exposing (mainLayout)
import Post exposing (BasicPost, Post, PostId, postDecoder)
import RemoteData
import UI exposing (viewFullPageSpinner)
import User


type alias Model =
    { id : PostId
    , post : WebDetailedData Post
    , selected : Maybe BasicPost
    }


init : PostId -> Maybe BasicPost -> ( Model, Cmd Msg )
init id selected =
    ( { id = id
      , post = RemoteData.Loading
      , selected = selected
      }
    , fetchPost id
    )


title : String
title =
    "Posts | Elm Blog"


view : Model -> Html Msg
view model =
    mainLayout
        (case model.post of
            RemoteData.NotAsked ->
                text ""

            RemoteData.Loading ->
                case model.selected of
                    Nothing ->
                        viewFullPageSpinner 8 1.5 True

                    Just basicPost ->
                        viewBasicAndSpinner basicPost

            RemoteData.Failure err ->
                -- TODO: show error
                div [] [ text ("Error: " ++ getHttpErrorMessage err) ]

            RemoteData.Success actualPost ->
                viewPost actualPost
        )
        []


viewBasicAndSpinner : BasicPost -> Html Msg
viewBasicAndSpinner basicPost =
    div
        [ class "flex-grow-1 container d-flex flex-column p-3"
        ]
        [ h1 [] [ text basicPost.title ]
        , viewFullPageSpinner 8 1.5 True
        ]


viewPost : Post -> Html Msg
viewPost post =
    div [ class "container p-3" ]
        [ h1 [] [ text post.title ]
        , viewPostExtraInfo post
        , p [] [ text post.content ]
        ]


viewPostExtraInfo : Post -> Html Msg
viewPostExtraInfo post =
    div [ class "d-flex align-items-center my-3" ]
        [ h5 [ class "text-info my-0 mx-2" ] [ a [ href (Category.getUrl post.category.id) ] [ text post.category.name ] ]
        , text "|"
        , h5 [ class "my-0 mx-2" ] [ a [ href (User.getUrl post.author.id) ] [ text ("by " ++ post.author.name) ] ]
        , text "|"
        , h5 [ class "my-0 mx-2 text-muted" ] [ DateTime.viewTimeAgo post.createdAt ]
        ]


type Msg
    = PostReceived (WebDetailedData Post)


fetchPost : PostId -> Cmd Msg
fetchPost id =
    Http.get
        { url = apiEndpoint (Post.getUrl id)
        , expect = Http.Detailed.expectJson (webDataFromResultDetailed >> PostReceived) postDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )


getBasicPostIfTheSame : Maybe BasicPost -> PostId -> Maybe BasicPost
getBasicPostIfTheSame selected postId =
    Maybe.andThen
        (\post ->
            if post.id == postId then
                Just post

            else
                Nothing
        )
        selected
