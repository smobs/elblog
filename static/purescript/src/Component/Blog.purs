module Component.Blog where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())
import Data.Traversable

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

type FState g = InstalledState Blog Article Query Article.Query g ArticleSlot
type FQuery = Coproduct Query (ChildF ArticleSlot Article.Query)

-- | The component definition9
blog :: forall a eff. (Functor a, MonadAff (BlogEffects eff) a) =>  Component (FState a) FQuery a
blog = parentComponent render eval
  where
    render :: State -> ParentHTML Article Query Article.Query a ArticleSlot
    render state =
      H.div [P.initializer \_ -> action Load]
      [ H.h1_
          [ H.text "Toby's Blog" ]
      , H.div_
        (map renderArticle state.articles)
      ]

    eval :: Natural Query (ParentDSL State Article.State Query Article.Query a ArticleSlot)
    eval (Load a) = do
      bs <-  liftH $ liftAff' getBlogs
      let ids = map (\(Article b) -> {id: b.id, title: b.title, contents: b.contents}) bs
      modify (\s -> s {articles = ids})
      traverse (\(Article b) -> query (ArticleSlot b.id) (action (Article.Load {title: b.title, contents:b.contents}))) bs
      pure a
    

    renderArticle :: {title :: String, contents :: String, id :: ArticleId} -> ParentHTML Article Query Article.Query a ArticleSlot
    renderArticle ar = H.slot (ArticleSlot (ar.id))
                            (\_ -> {component: article,
                                   initialState: initialArticle ar.id ar.title ar.contents})

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

