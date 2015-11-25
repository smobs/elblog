module Blog.Entry where

import Html exposing(..)
import Html.Events exposing(..)
import Signal


type alias Model = {title : String, body : String, open : Bool}

type Action = Toggle

update : Action -> Model -> Model
update ac m =
  case ac of
    Toggle -> {m | open <- not m.open}

view : Signal.Address Action -> Model -> Html
view addr {title, body, open} = div [onClick addr Toggle] 
                                (if open
                                 then [ text title
                                     , br [] []
                                     , text body]
                           else [text title])

init : String -> String -> Model
init t b = {title = t, body = b, open = True}
