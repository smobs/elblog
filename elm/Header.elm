module Header where

import Html exposing(..)
import Html.Events exposing (onClick)
import Signal
import List

view : Signal.Address a -> List (String, a) -> Html
view a ps = 
  let f x = navLink a x  in
  div []  (List.map f ps)


navLink : Signal.Address a -> (String, a) -> Html
navLink adr (n, p) = button [onClick adr p] [text n]
