module Main exposing (..)

import Html
import Add exposing (..)

main : Html.Html msg
main =
  Html.text (String.fromInt (Add.add 1 2))
