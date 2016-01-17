module Component.Article where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Control.Monad.Aff (Aff())

import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as Ajax

import Model

type State = Article

data Query a = Toggle a
             | Load Int a

type ArticleEffects eff = HalogenEffects (ajax :: AJAX | eff)

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
    modify (\s -> s {contents = c})
    pure next

getBlog :: forall eff. Int -> Aff (ajax :: AJAX | eff) String
getBlog id =
  let url = "api/blog/" ++ show id in
  do
    res <- Ajax.get url
    return res.response



