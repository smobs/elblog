module Component.Blog where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Model
import Component.Article (article)
import qualified Component.Article as Article
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Class
import Network.HTTP.Affjax (AJAX())
import qualified Network.HTTP.Affjax as Ajax

import Data.Foreign
import Data.Foreign.Class
import Data.Either (Either(..))

newtype ArticleSlot = ArticleSlot ArticleId

derive instance articleSlot :: Generic ArticleSlot
instance eqArticleSlot :: Eq ArticleSlot where eq = gEq
instance ordArticleSlot :: Ord ArticleSlot where compare = gCompare

type BlogEffects eff = HalogenEffects(ajax :: AJAX | eff)

type State = Blog
data Query a =
  Load a

type FState g = InstalledState Blog (Article.FState g) Query Article.FQuery g ArticleSlot
type FQuery = Coproduct Query (ChildF ArticleSlot Article.FQuery)
type BlogDSL g = ParentDSL State (Article.FState g) Query Article.FQuery g ArticleSlot
type BlogHTML g = ParentHTML (Article.FState g) Query Article.FQuery g ArticleSlot
-- | The component definition9
blog :: forall a eff. (Functor a, MonadAff (BlogEffects eff) a) =>  Component (FState a) FQuery a
blog = parentComponent render eval
  where
    render :: State -> BlogHTML a
    render state =
      H.div [P.initializer \_ -> action Load]
      [ H.h1_
          [ H.text "Toby's Blog" ]
      , H.div_
        (map renderArticle state.articles)
      ]

    eval :: Natural Query (BlogDSL a)
    eval (Load a) = do
      bs <-  liftH $ liftAff' $ getBlogs
      let ids = map (\(Article b) -> {id: b.id, title: b.title, contents: b.contents}) bs
      modify (\s -> s {articles = ids})
      pure a
    

    renderArticle :: {title :: String, contents :: String, id :: ArticleId} -> BlogHTML a
    renderArticle ar = H.slot
                       (ArticleSlot (ar.id))
                       \_ -> { initialState: installedState $ initialArticle ar.id ar.title ar.contents
                             , component: article}

getBlogs :: forall eff. Aff (ajax :: AJAX | eff) (Array Article)
getBlogs = 
  let url = "api/blogs" in
  do
    resp <- Ajax.get url
    return case
      readJSON resp.response :: F (Array Article)
      of
        Right ids -> ids
        Left err -> [Article {title: "ERROR", contents: show err, visible: true, id: -1}]

