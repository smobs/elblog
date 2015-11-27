module Main where
import StartApp
import Task
import Effects exposing (Never)
import Html exposing (..)
import History
import Routing exposing(..)
import Header

type Model = Model {page : Page}


app = StartApp.start
      { init = init
      , update = update
      , inputs = [urlNavigation]
      , view = view
      }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

init = ( Model
         {
           page = defaultPage
         } 
       , Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update a (Model {page})  = (Model {page = updateBody a page}, Effects.none)

view : Signal.Address Action ->  Model -> Html
view a (Model {page}) = body [] [ (viewHeader a)
                                 , (viewBody a page)
                              ]

viewHeader : Signal.Address Action -> Html
viewHeader a  = Header.view 
                (Signal.forwardTo a Navigate)
                navRoutes

urlNavigation : Signal Action
urlNavigation = Signal.map (\s -> Navigate (routes s)) History.hash
