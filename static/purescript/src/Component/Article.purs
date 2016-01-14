module Component.Article where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Model

type State = Article

data Query a = Null a

article :: forall g. (Functor g) => Component State Query g
article = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.text state
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Null next) = do
    pure next
