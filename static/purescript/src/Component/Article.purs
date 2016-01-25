module Component.Article where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Aff (Aff())
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as Ajax

import Data.Foreign
import Data.Foreign.Class

import Model

type State = Article

data Query a = Toggle a
             | Load {title :: String, contents :: String} a

type ArticleEffects eff = HalogenEffects (ajax :: AJAX | eff)
data ArticleContents = ArticleContents {title :: String, contents :: String}

article :: forall eff. Component State Query (Aff (ArticleEffects eff))
article = component render eval
  where

  render :: State -> ComponentHTML Query
  render (Article state) =
    let title = H.h2 [E.onClick (E.input_ Toggle)
                     ]
                [H.text state.title] in
    H.div_
      if state.visible
      then
        [ title
        , H.text state.contents
        ]
      else
         [title]

  eval ::  Natural Query (ComponentDSL State Query (Aff (ArticleEffects eff)))
  eval (Toggle next) = do
    modify (\(Article s)-> Article (s {visible = not s.visible}))
    pure next
  eval (Load ar next) = do
    modify (\(Article s) -> Article  (s {contents = ar.contents, title = ar.title}))
    pure next
