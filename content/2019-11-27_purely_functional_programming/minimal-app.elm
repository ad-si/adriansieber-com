module Main exposing (main)

import Browser
import Html exposing (Html, text)


type alias Model =
    { body : Html Msg }


type Msg
    = DoNothing


initialModel : Model
initialModel =
    { body = text "test" }


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view model =
    model.body


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
