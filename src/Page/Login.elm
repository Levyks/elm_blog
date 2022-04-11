module Page.Login exposing (Model, Msg, init, title, update, view)

import Alert exposing (AlertParams, AlertType(..))
import Css exposing (..)
import Form
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onSubmit)
import Http
import Http.Detailed
import HttpHelper exposing (apiEndpoint, getHttpErrorMessage)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout exposing (mainLayout)
import UI exposing (viewSpinner)


type alias Model =
    { email : String
    , password : String
    , loading : Bool
    , error : Maybe String
    }


type alias LoginSuccessfulResult =
    { message : String
    , token : String
    }


init : ( Model, Cmd Msg )
init =
    ( { email = ""
      , password = ""
      , loading = False
      , error = Nothing
      }
    , Cmd.none
    )


formCard : Model -> Html Msg
formCard model =
    div
        [ class "card"
        , css
            [ width (px 400)
            , maxWidth (calc (pct 100) minus (px 20))
            ]
        ]
        [ form
            [ onSubmit HandleSubmit
            ]
            [ div
                [ class "card-header" ]
                [ h2 [ class "m-0" ] [ text "Login" ]
                ]
            , div [ class "card-body" ]
                [ Form.input
                    { label = "E-mail"
                    , id = "email"
                    , type_ = "email"
                    , value = model.email
                    , required = False
                    , disabled = model.loading
                    , onInput = HandleEmailInput
                    }
                , Form.input
                    { label = "Password"
                    , id = "password"
                    , type_ = "password"
                    , value = model.password
                    , required = False
                    , disabled = model.loading
                    , onInput = HandlePasswordInput
                    }
                ]
            , div
                [ class "card-footer" ]
                [ button
                    [ class "btn btn-primary d-flex justify-content-end align-items-center ms-auto"
                    , Html.Styled.Attributes.disabled model.loading
                    ]
                    [ if model.loading then
                        viewSpinner 4 0.75

                      else
                        text ""
                    , text "Login"
                    ]
                ]
            ]
        ]


getAlertsList : Model -> List (AlertParams Msg)
getAlertsList model =
    case model.error of
        Nothing ->
            []

        Just msg ->
            [ AlertParams Danger msg True False (Just HandleErrorClosed) ]


title : String
title =
    "Login | Elm Blog"


view : Model -> Html Msg
view model =
    mainLayout
        (div
            [ class "container"
            , css
                [ displayFlex
                , flexDirection column
                , alignItems center
                , justifyContent center
                , height (pct 100)
                , padding (px 20)
                ]
            ]
            [ formCard model
            ]
        )
        (getAlertsList model)


type Msg
    = HandleEmailInput String
    | HandlePasswordInput String
    | HandleSubmit
    | SubmitResultReceived (Result (Http.Detailed.Error String) ( Http.Metadata, LoginSuccessfulResult ))
    | HandleErrorClosed


requestEncoder : Model -> Encode.Value
requestEncoder model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        ]


resultDecoder : Decode.Decoder LoginSuccessfulResult
resultDecoder =
    Decode.map2 LoginSuccessfulResult
        (Decode.field "message" Decode.string)
        (Decode.field "token" Decode.string)


submitLogin : Model -> Cmd Msg
submitLogin model =
    Http.post
        { url = apiEndpoint "/auth/login"
        , body = Http.jsonBody <| requestEncoder model
        , expect = Http.Detailed.expectJson SubmitResultReceived resultDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        HandleEmailInput email ->
            ( { model | email = email }, Cmd.none )

        HandlePasswordInput password ->
            ( { model | password = password }, Cmd.none )

        HandleSubmit ->
            ( { model | loading = True, error = Nothing }, submitLogin model )

        SubmitResultReceived (Ok ( _, _ )) ->
            -- TODO: Store token in local storage
            ( { model | loading = False }, Cmd.none )

        SubmitResultReceived (Err err) ->
            ( { model | loading = False, error = Just (getHttpErrorMessage err) }, Cmd.none )

        HandleErrorClosed ->
            let
                _ =
                    Debug.log "teste" "teste"
            in
            ( model, Cmd.none )
