module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Css exposing (url)
import Html.Styled as Html
import Page.Login as Login
import Route exposing (Route(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : Page
    , title : String
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | HomePage
    | LoginPage Login.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | LoginPageMsg Login.Msg


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds, currentTitle ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none, "Not Found" )

                Route.Home ->
                    ( HomePage, Cmd.none, "Home" )

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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , title = ""
            , navKey = navKey
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
            Html.text "404"

        HomePage ->
            Html.text "Home"

        LoginPage pageModel ->
            Login.view pageModel |> Html.map LoginPageMsg


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
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( LoginPageMsg subMsg, LoginPage pageModel ) ->
            let
                ( updatedPageModel, pageCmds ) =
                    Login.update subMsg pageModel
            in
            ( { model | page = LoginPage updatedPageModel }
            , Cmd.batch [ Cmd.map LoginPageMsg pageCmds ]
            )

        ( _, _ ) ->
            initCurrentPage ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
