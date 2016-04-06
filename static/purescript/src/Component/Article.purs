module Component.Article where

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Data.Functor.Coproduct (Coproduct())

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Html
import Text.Markdown.SlamDown.Parser

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

type FState g = InstalledState State SlamDownState Query SlamDownQuery g MarkdownSlot
type FQuery = Coproduct Query (ChildF MarkdownSlot SlamDownQuery)
type ArticleHTML g = ParentHTML SlamDownState Query SlamDownQuery g MarkdownSlot
type ArticleDSL g = ParentDSL State SlamDownState Query SlamDownQuery g MarkdownSlot

article :: forall g. (Functor g) => Component (FState g) FQuery g
article = parentComponent render eval
  where

  render :: State -> ArticleHTML g
  render (Article state) =
    let title = H.button [ E.onClick (E.input_ Toggle)
                         , P.classes $ map className ["pure-u-1-1", "pure-button", "pure-button-primary"]]
                [H.text state.title] in
    H.div [P.class_ (className "pure-g")]
      if state.visible
      then
        [ title
        , H.div [P.class_ (className "pure-u-1-1")] [H.slot MarkdownSlot \_ -> { initialState: makeSlamDownState $ parseMd state.contents
                                                                               , component: slamDownComponent {browserFeatures: defaultBrowserFeatures, formName: "article-markdown-form"}}]
        ]
      else
         [title]

  eval ::  Natural Query (ArticleDSL g)
  eval (Toggle next) = do
    modify (\(Article s)-> Article (s {visible = not s.visible}))
    pure next
  eval (Load ar next) = do
    modify (\(Article s) -> Article  (s {contents = ar.contents, title = ar.title}))
    pure next
