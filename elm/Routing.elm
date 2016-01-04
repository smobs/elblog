module Routing where


import Html exposing (..)

import Home
import Blog


type Action = Navigate Page 
            | HomeAction Home.Action
            | BlogAction Blog.Action

type Page = Home Home.Model 
          | Blog Blog.Model


routes : Route Page
routes = Router.match
         ["/" :-> (\_ -> Home Home.init)
         , "#/blog" :-> (\_ -> Blog Blog.init)
         ] (\_ -> Home Home.init) 

navRoutes : List (String, Page)
navRoutes = [ ("Home", Home Home.init)
            , ("Blog", Blog Blog.init)]

defaultPage : Page
defaultPage = Home Home.init

updateBody : Action -> Page -> Page
updateBody a page = case (a, page) of 
                (Navigate page, _) -> 
                  page
                (HomeAction a', Home m') ->
                  Home (Home.update a' m')
                (BlogAction a', Blog m') ->
                  Blog (Blog.update a' m')

viewBody : Signal.Address Action -> Page -> Html
viewBody a p = case p of 
                 (Home m) -> Home.view (Signal.forwardTo a HomeAction) m
                 (Blog m) -> Blog.view (Signal.forwardTo a BlogAction) m
