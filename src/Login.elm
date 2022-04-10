module Login exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, for, id, type_, value)
import Html.Styled.Events exposing (onInput)


type alias Model =
    { email : String
    , password : String
    , rememberMe : Bool
    , loading : Bool
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , rememberMe = False
    , loading = False
    }


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


formCard : Model -> Html Msg
formCard model =
    div
        [ class "card" ]
        [ div
            [ class "card-header" ]
            [ h2 [ class "m-0" ] [ text "Login" ]
            ]
        , div [ class "card-body" ]
            [ form []
                [ div [ class "mb-3" ]
                    [ label [ for "email" ] [ text "Email" ]
                    , input
                        [ class "form-control"
                        , id "email"
                        , type_ "email"
                        , value model.email
                        , onInput HandleEmailInput
                        ]
                        []
                    ]
                , div [ class "mb-3" ]
                    [ label [ for "password" ] [ text "Password" ]
                    , input
                        [ class "form-control"
                        , id "password"
                        , type_ "password"
                        , value model.password
                        , onInput HandlePasswordInput
                        ]
                        []
                    ]
                ]
            ]
        , div
            [ class "card-footer text-end" ]
            [ button
                [ class "btn btn-primary"
                ]
                [ text "Login" ]
            ]
        ]


type Msg
    = HandleEmailInput String
    | HandlePasswordInput String
    | HandleSubmit


main =
    view initialModel |> toUnstyled
