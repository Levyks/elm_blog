module Login exposing (main)

import Browser
import Css exposing (..)
import Debug exposing (log)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick, onSubmit)
import Http
import Http.Detailed
import Json.Decode as Decode
import Json.Encode as Encode
import Misc exposing (apiUrl, formInput, getHttpErrorMessage)


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


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , loading = False
    , error = Nothing
    }


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
                [ formInput
                    { label = "E-mail"
                    , id = "email"
                    , type_ = "email"
                    , value = model.email
                    , required = False
                    , disabled = model.loading
                    , onInput = HandleEmailInput
                    }
                , formInput
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
                        div
                            [ class "spinner-border me-2"
                            , css
                                [ width (px 20)
                                , height (px 20)
                                , fontSize (rem 0.75)
                                ]
                            ]
                            [ span
                                [ class "visually-hidden"
                                ]
                                [ text "Loading..." ]
                            ]

                      else
                        text ""
                    , text "Login"
                    ]
                ]
            ]
        ]


alertsDiv : Model -> Html Msg
alertsDiv model =
    case model.error of
        Nothing ->
            div [] []

        Just msg ->
            div
                [ class "alert alert-danger alert-dismissible"
                , css
                    [ width (pct 100)
                    ]
                ]
                [ text msg
                , button
                    [ class "btn-close"
                    , onClick HandleErrorAlertClose
                    ]
                    []
                ]


view : Model -> Html Msg
view model =
    div
        [ class "container"
        , css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent spaceBetween
            , height (pct 100)
            , padding (px 20)
            ]
        ]
        [ alertsDiv model
        , formCard model
        , div [] []
        ]


type Msg
    = HandleEmailInput String
    | HandlePasswordInput String
    | HandleSubmit
    | HandleErrorAlertClose
    | SubmitResultReceived (Result (Http.Detailed.Error String) ( Http.Metadata, LoginSuccessfulResult ))


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
        { url = apiUrl ++ "/auth/login"
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

        HandleErrorAlertClose ->
            ( { model | error = Nothing }, Cmd.none )

        HandleSubmit ->
            ( { model | loading = True, error = Nothing }, submitLogin model )

        SubmitResultReceived (Ok ( _, body )) ->
            -- TODO: Store token in local storage
            ( { model | loading = False }, Cmd.none )

        SubmitResultReceived (Err err) ->
            ( { model | loading = False, error = Just (getHttpErrorMessage err) }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
