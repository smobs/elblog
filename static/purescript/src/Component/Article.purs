module Component.Article where

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.CSS.PureCSS as Pure
import Data.Functor.Coproduct (Coproduct())

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Halogen.Component
import Text.Markdown.SlamDown.Parser

import Data.Maybe (Maybe(..))
import Data.Either(Either(..))

import Model

type State = Article

data Query a = Toggle a
             | Load {title :: String, contents :: String} a

data ArticleContents = ArticleContents {title :: String, contents :: String}

data MarkdownSlot = MarkdownSlot

instance ordMarkdownSlot :: Ord MarkdownSlot where
  compare _ _ = EQ

instance eqMarkdownSlot :: Eq MarkdownSlot where
  eq _ _ = true

type SlamS = SlamDownState String
type SlamQ = SlamDownQuery String

type FState g = ParentState State SlamS Query SlamQ g MarkdownSlot
type FQuery = Coproduct Query (ChildF MarkdownSlot SlamQ)
type ArticleHTML g = ParentHTML SlamS Query SlamQ g MarkdownSlot
type ArticleDSL g = ParentDSL State SlamS Query SlamQ g MarkdownSlot

article :: forall g. (Functor g) => Component (FState g) FQuery g
article = parentComponent {render, eval, peek: Nothing}
  where

  render :: State -> ArticleHTML g
  render (Article state) =
    let title = H.button [ E.onClick (E.input_ Toggle)
                         , P.classes [Pure.u 1 1, Pure.button, Pure.buttonPrimary]]
                [H.text state.title] in
    H.div [P.class_ Pure.grid]
      if state.visible
      then
        [ title
        , H.div [P.class_ $ Pure.u 1 1] [H.slot MarkdownSlot \_ ->
                                                      { initialState: (case (parseMd state.contents) of
                                                           Right slam -> replaceDocument slam
                                                           Left _ -> id)
                                                           emptySlamDownState
                                                      , component: slamDownComponent {browserFeatures: defaultBrowserFeatures, formName: "article-markdown-form"}}]
        ]
      else
         [title]

  eval ::  Query ~> (ArticleDSL g)
  eval (Toggle next) = do
    modify (\(Article s)-> Article (s {visible = not s.visible}))
    pure next
  eval (Load ar next) = do
    modify (\(Article s) -> Article  (s {contents = ar.contents, title = ar.title}))
    pure next
