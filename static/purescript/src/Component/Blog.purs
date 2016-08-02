module Component.Blog where

import Prelude

import Data.Generic (class Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Model
import Component.Article (article)
import Component.Article as Article
import Control.Monad.Aff 
import Control.Monad.Aff.Class
import Control.Monad.Aff.Free
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax as Ajax

import Data.Foreign
import Data.Foreign.Class
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

newtype ArticleSlot = ArticleSlot ArticleId

derive instance articleSlot :: Generic ArticleSlot
instance eqArticleSlot :: Eq ArticleSlot where eq = gEq
instance ordArticleSlot :: Ord ArticleSlot where compare = gCompare

type BlogEffects eff = HalogenEffects(ajax :: AJAX | eff)

type State = Blog
data Query a =
  Load a

type FState g = ParentState Blog (Article.FState g) Query Article.FQuery g ArticleSlot
type FQuery = Coproduct Query (ChildF ArticleSlot Article.FQuery)
type BlogDSL g = ParentDSL State (Article.FState g) Query Article.FQuery g ArticleSlot
type BlogHTML g = ParentHTML (Article.FState g) Query Article.FQuery g ArticleSlot
-- | The component definition9
blog :: forall a eff. (Functor a, MonadAff (BlogEffects eff) a) =>  Component (FState a) FQuery a
blog = lifecycleParentComponent {render, eval, peek: Nothing, initializer: Just (action Load), finalizer: Nothing}
  where
    render :: State -> BlogHTML a
    render state =
      H.div_
      (map renderArticle state.articles)
      

    eval :: Query ~> (BlogDSL a)
    eval (Load a) = do
      bs <-  liftH <<< liftH <<< liftAff $ getBlogs
      let ids = map (\(Article b) -> {id: b.id, title: b.title, contents: b.contents}) bs
      modify (\s -> s {articles = ids})
      pure a
    

    renderArticle :: {title :: String, contents :: String, id :: ArticleId} -> BlogHTML a
    renderArticle ar = H.slot
                       (ArticleSlot (ar.id))
                       \_ -> { initialState: parentState $ initialArticle ar.id ar.title ar.contents
                             , component: article}

getBlogs :: forall eff. Aff (ajax :: AJAX | eff) (Array Article)
getBlogs = 
  let url = "api/blogs" in
  do
    resp <- Ajax.get url
    pure case
      readJSON resp.response :: F (Array Article)
      of
        Right ids -> ids
        Left err -> [Article {title: "ERROR", contents: show err, visible: true, id: -1}]

