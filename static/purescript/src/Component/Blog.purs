module Component.Blog where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Model

-- | The state of the component
type State = Blog

-- | The query algebra for the component
data Query a =
  GetState (State -> a)

-- | The component definition
blog :: forall g. (Functor g) => Component State Query g
blog = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Awesome Blog" ]
      , H.ul_
        (map renderArticle state.articles)
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (GetState continue) = do
    value <- get
    pure (continue value)

  renderArticle :: Article -> ComponentHTML Query
  renderArticle article =
    H.li_[
      H.text article]

