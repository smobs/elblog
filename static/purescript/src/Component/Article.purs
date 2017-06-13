module Component.Article where

import Prelude
import Halogen

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


article :: forall g . Component H.HTML Query Article Void g
article = parentComponent { initialState: id
                          , render
                          , eval
                          , receiver: const Nothing}
  where
  render :: State -> ParentHTML Query SlamQ MarkdownSlot g
  render (Article state) =
    let title = H.button [ E.onClick (E.input_ Toggle)
                         , P.classes [Pure.u 1 1, Pure.button, Pure.buttonPrimary]]
                [H.text state.title] 
    in
      H.div [P.class_ Pure.grid]
        if state.visible
        then [ title
             , H.div [P.class_ $ Pure.u 1 1] 
                [ H.slot MarkdownSlot 
                    (slamDownComponent {browserFeatures: defaultBrowserFeatures, formName: "article-markdown-form"}) 
                    unit
                    (const Nothing)]
             ]
        else [title]
  
  eval ::  Query ~> ParentDSL State Query SlamQ MarkdownSlot Void g
  eval (Toggle next) = do
    modify (\(Article s)-> Article (s {visible = not s.visible}))
    pure next
  eval (Load ar next) = do
    modify (\(Article s) -> Article  (s {contents = ar.contents, title = ar.title}))
    pure next
