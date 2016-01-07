module Main where
import StartApp
import Task
import Effects exposing (Never)
import Html exposing (..)
import Blog

type alias Model = Blog.Model

type alias Action = Blog.Action

app = StartApp.start
      { init = init
      , update = update
      , inputs = []
      , view = view
      }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

init = ( Blog.init
       , Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update  a m = (Blog.update a m, Effects.none) 

view : Signal.Address Action ->  Model -> Html
view = Blog.view


