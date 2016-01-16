module Component.Blog where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Model
import Component.Article (article)
import qualified Component.Article as Article
import Control.Monad.Aff (Aff())

type State = Blog
-- | The state of the component
type FState g = InstalledState Blog Article Query Article.Query g ArticleSlot

newtype ArticleSlot = ArticleSlot ArticleId

derive instance articleSlot :: Generic ArticleSlot
instance eqArticleSlot :: Eq ArticleSlot where eq = gEq
instance ordArticleSlot :: Ord ArticleSlot where compare = gCompare

type FQuery = Coproduct Query (ChildF ArticleSlot Article.Query)

-- | The query algebra for the component
data Query a =
  GetState (State -> a)

-- | The component definition9
blog :: forall eff. Component (FState (Aff(Article.ArticleEffects eff))) FQuery (Aff(Article.ArticleEffects eff))
blog = parentComponent render eval
  where

  render :: State -> ParentHTML Article Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot
  render state =
    H.div_
      [ H.h1_
          [ H.text "Awesome Blog" ]
      , H.div_
        (map renderArticle state.articles)
      ]

  eval :: Natural Query (ParentDSL State Article.State Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot)
  eval (GetState continue) = do
    value <- get
    pure (continue value)

  renderArticle :: ArticleId -> ParentHTML Article Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot
  renderArticle articleId = H.slot (ArticleSlot articleId) \_ -> {component: article, initialState: initialArticle articleId}  

