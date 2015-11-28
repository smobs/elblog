module Blog where

import Blog.Entry as Entry
import Html exposing(..)
import Html.Events exposing(onClick)
import Html.Attributes exposing(style, class)
import List
import Signal


type alias ID = Int 
type alias Model = {nextID : ID, blogs : List (ID, Entry.Model)}

type Action = NewBlog String String | EntryAction ID Entry.Action

init : Model
init = {nextID = 0, blogs = []}

update : Action -> Model -> Model
update a m = 
  let {nextID, blogs} = m in
  case a of
    (NewBlog title content) -> {nextID = nextID + 1, blogs = (nextID, Entry.init title content) :: blogs}
    (EntryAction id ac) -> {m | blogs <- blogs 
                              |> List.map (\(i, m) -> 
                                (i, if i == id then Entry.update ac m else m))
                           }
             
view : Signal.Address Action -> Model -> Html
view adr m = div [] [ blogEntries adr m.blogs
                    , button
                      [ onClick adr (NewBlog "New" "Stuff")
                      , style newBlogButtonStyle
                      , class "pure-button"
                      ]
                      [text "New"]
                    ]


blogEntries : Signal.Address Action -> List (Int, Entry.Model) -> Html
blogEntries addr m = m
       |> List.map (\(i, h) -> h 
                   |> Entry.view (Signal.forwardTo addr (EntryAction i)) 
                   |> listItem) 
       |> ul []

listItem : Html -> Html
listItem h = li [] [h]

newBlogButtonStyle : List (String, String)
newBlogButtonStyle = [("background-color", "green")]
