module Component.Page where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Control.Monad.Aff.Class

import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct())

import Component.Blog as Blog
import Component.Jenny as Jenny
import Model

type State = Page
type PageEffects a = Blog.BlogEffects a

data Query a =
  Navigate Page a

data BlogSlot = BlogSlot

instance ordBlogSlot :: Ord BlogSlot where
  compare _ _ = EQ

instance eqBlogSlot :: Eq BlogSlot where
  eq _ _ = true

data JennySlot = JennySlot
instance ordJennySlot :: Ord JennySlot where
  compare _ _ = EQ

instance eqJennySlot :: Eq JennySlot where
  eq _ _ = true



type ChildState g = Either (Blog.FState g) Jenny.State
type ChildQuery = Coproduct (Blog.FQuery) (Jenny.Query )
type ChildSlot = Either BlogSlot JennySlot

type FState g = ParentState State (ChildState g) Query ChildQuery g ChildSlot
type FQuery = Coproduct Query (ChildF ChildSlot ChildQuery)
type PageDSL g = ParentDSL State (ChildState g) Query ChildQuery g ChildSlot
type PageHTML g = ParentHTML (ChildState g) Query ChildQuery g ChildSlot

page :: forall a eff . (Functor a, MonadAff (PageEffects eff) a) => Component (FState a) FQuery a
page =
  parentComponent {render, eval, peek: Nothing}
  where
    render :: State -> PageHTML a
    render state = H.text "Test"

    eval :: Natural Query (PageDSL a)
    eval (Navigate _ a) =
      pure a
