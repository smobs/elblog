module Component.Blog where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Model
import Component.Article (article)
import qualified Component.Article as Article
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as Ajax

import Data.Foreign
import Data.Foreign.Class
import Data.Either (Either(..))


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
  Load a
    
-- | The component definition9
blog :: forall eff. Component (FState (Aff(Article.ArticleEffects eff))) FQuery (Aff(Article.ArticleEffects eff))
blog = parentComponent render eval
  where

  render :: State -> ParentHTML Article Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot
  render state =
    H.div [P.initializer \_ -> action Load]
      [ H.h1_
          [ H.text "Toby's Blog" ]
      , H.div_
        (map renderArticle state.articles)
      ]

  eval :: Natural Query (ParentDSL State Article.State Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot)
  eval (Load a) = do
    ids <- liftH $ liftAff' getBlogIds
    modify (\s -> s {articles = ids})
    pure a
    

  renderArticle :: ArticleId -> ParentHTML Article Query Article.Query (Aff(Article.ArticleEffects eff)) ArticleSlot
  renderArticle articleId = H.slot (ArticleSlot articleId) \_ -> {component: article, initialState: initialArticle articleId}

getBlogIds :: forall eff. Aff (ajax :: AJAX | eff) (Array ArticleId)
getBlogIds = 
let url = "api/blogs" in
  do
    resp <- Ajax.get url
    return case
      readJSON resp.response :: F (Array Int)
      of
        Right ids -> ids
        Left _ -> []
