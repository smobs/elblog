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
             | Load Int a

type ArticleEffects eff = HalogenEffects (ajax :: AJAX | eff)
data ArticleContents = ArticleContents {title :: String, contents :: String}

article :: forall eff. Component State Query (Aff (ArticleEffects eff))
article = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    let title = H.h2 [E.onClick (E.input_ Toggle)
                     , P.initializer \_ -> action (Load state.id)]
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
    modify (\s -> s {visible = not s.visible})
    pure next
  eval (Load id next) = do
    c <- liftAff' $ getBlog id
    modify (\s -> s {contents = c.contents, title = c.title})
    pure next

getBlog :: forall eff. Int -> Aff (ajax :: AJAX | eff) {title :: String, contents :: String}
getBlog id =
  let url = "api/blog/" ++ show id in
  do
    res <- Ajax.get url
    return case (do
      title <- readProp "title" res.response
      contents <- readProp "content" res.response
      return {title : title, contents: contents}) of
             Right c -> c 
             Left e -> {title : "Invalid blog", contents : show e}
