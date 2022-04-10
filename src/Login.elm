module Login exposing (main)

import Browser
import Css exposing (..)
import Debug exposing (log)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onSubmit)
import Misc exposing (formInput)


type alias Model =
    { email : String
    , password : String
    , loading : Bool
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , loading = False
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
                    , required = True
                    , onInput = HandleEmailInput
                    }
                , formInput
                    { label = "Password"
                    , id = "password"
                    , type_ = "password"
                    , value = model.password
                    , required = True
                    , onInput = HandlePasswordInput
                    }
                ]
            , div
                [ class "card-footer text-end" ]
                [ button
                    [ class "btn btn-primary"
                    ]
                    [ text "Login" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent center
            , height (pct 100)
            ]
        ]
        [ formCard model
        ]


type Msg
    = HandleEmailInput String
    | HandlePasswordInput String
    | HandleSubmit


update : Msg -> Model -> Model
update message model =
    case message of
        HandleEmailInput email ->
            { model | email = email }

        HandlePasswordInput password ->
            { model | password = password }

        HandleSubmit ->
            log "Submitting" model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view >> toUnstyled
        , update = update
        }
