module Page.Home exposing (main)

import Browser
import Html.Styled exposing (..)


type alias Model =
    {}


view : Model -> Html msg
view _ =
    div []
        [ h1 [] [ text "Hello World!" ]
        ]


update : msg -> Model -> Model
update _ model =
    model


main : Program () Model msg
main =
    Browser.sandbox
        { init = {}
        , view = view >> toUnstyled
        , update = update
        }
