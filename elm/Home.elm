module Home where
import Html exposing(..)
import Html.Events exposing(onClick)

import Signal

type alias Model = String
type Action = Update Model

init : Model
init = "Hello"

update : Action -> Model -> Model
update (Update s) _ = s

view : Signal.Address Action -> Model -> Html
view a s = div [] [ text "Welcome to my amazing website"
                , br [] []
                , text s
                , button [onClick a (Update "Change")] [text "Change"]
                ]

