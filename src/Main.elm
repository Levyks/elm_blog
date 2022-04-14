module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Css exposing (url)
import Html.Styled as Html
import Html.Styled.Attributes exposing (href)
import Page.ListPosts as ListPosts
import Page.Login as Login
import Page.ViewPost as ViewPost
import Post exposing (BasicPost)
import Route exposing (Route(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : Page
    , title : String
    , navKey : Nav.Key
    , baseUrl : String
    , selectedPost : Maybe BasicPost
    }


type Page
    = NotFoundPage
    | ListPostsPage ListPosts.Model
    | ViewPostPage ViewPost.Model
    | LoginPage Login.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | LoginPageMsg Login.Msg
    | ListPostsPageMsg ListPosts.Msg
    | ViewPostPageMsg ViewPost.Msg


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds, currentTitle ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none, "Not Found" )

                Route.ListPosts page search ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init page search
                    in
                    ( ListPostsPage pageModel, Cmd.map ListPostsPageMsg pageCmds, ListPosts.title )

                Route.ViewPost postId ->
                    let
                        ( pageModel, pageCmds ) =
                            ViewPost.init postId (ViewPost.getBasicPostIfTheSame model.selectedPost postId)
                    in
                    ( ViewPostPage pageModel, Cmd.map ViewPostPageMsg pageCmds, ViewPost.title )

                Route.Login ->
                    let
                        ( pageModel, pageCmds ) =
                            Login.init
                    in
                    ( LoginPage pageModel, Cmd.map LoginPageMsg pageCmds, Login.title )
    in
    ( { model | page = currentPage, title = currentTitle }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init baseUrl url navKey =
    let
        model =
            { route = Route.parseUrl baseUrl url
            , page = NotFoundPage
            , title = ""
            , navKey = navKey
            , baseUrl = baseUrl
            , selectedPost = Nothing
            }
    in
    initCurrentPage ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = model.title
    , body = [ currentView model |> Html.toUnstyled ]
    }


currentView : Model -> Html.Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            Html.a [ href "posts/" ] [ Html.text "404" ]

        ListPostsPage pageModel ->
            ListPosts.view pageModel |> Html.map ListPostsPageMsg

        LoginPage pageModel ->
            Login.view pageModel |> Html.map LoginPageMsg

        ViewPostPage pageModel ->
            ViewPost.view pageModel |> Html.map ViewPostPageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl model.baseUrl url

                selected =
                    case ( newRoute, model.page ) of
                        ( Route.ViewPost postId, ListPostsPage listModel ) ->
                            ListPosts.getPostById listModel postId

                        _ ->
                            Nothing
            in
            if newRoute /= model.route then
                ( { model | route = newRoute, selectedPost = selected }, Cmd.none )
                    |> initCurrentPage

            else
                ( model, Cmd.none )

        ( LoginPageMsg subMsg, LoginPage pageModel ) ->
            let
                ( updatedPageModel, pageCmds ) =
                    Login.update subMsg pageModel
            in
            ( { model | page = LoginPage updatedPageModel }
            , Cmd.batch [ Cmd.map LoginPageMsg pageCmds ]
            )

        ( ListPostsPageMsg subMsg, ListPostsPage pageModel ) ->
            let
                ( updatedPageModel, pageCmds ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPostsPage updatedPageModel }
            , Cmd.batch [ Cmd.map ListPostsPageMsg pageCmds ]
            )

        ( ViewPostPageMsg subMsg, ViewPostPage pageModel ) ->
            let
                ( updatedPageModel, pageCmds ) =
                    ViewPost.update subMsg pageModel
            in
            ( { model | page = ViewPostPage updatedPageModel }
            , Cmd.batch [ Cmd.map ViewPostPageMsg pageCmds ]
            )

        _ ->
            initCurrentPage ( model, Cmd.none )


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
