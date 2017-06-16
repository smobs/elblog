module Component.Blog where

import Prelude
import Halogen
import Model (Article(..), ArticleId, BlogState, initialArticle, initialBlog)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)

import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Component.Article as Article
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Component.Article (article)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import Servant.PureScript.Settings (SPSettings_(..))
import WebAPI (getApiBlogs, SPParams_(..))
import System.BlogRepository (Blog(..))
import WebAPI.Settings

newtype ArticleSlot = ArticleSlot ArticleId

derive instance articleSlot :: Generic ArticleSlot
instance eqArticleSlot :: Eq ArticleSlot where eq = gEq
instance ordArticleSlot :: Ord ArticleSlot where compare = gCompare

type BlogEffects eff = (ajax :: AJAX | eff)
type BlogAff eff = Aff (BlogEffects eff)

type State = BlogState
data Query a =
  Load a

type BlogHTML eff = ParentHTML Query Article.Query ArticleSlot (BlogAff eff)
type BlogDSL eff = ParentDSL State Query Article.Query ArticleSlot Void (BlogAff eff)

-- | The component definition9
blog :: forall eff. Component H.HTML Query Unit Void (BlogAff eff)
blog = lifecycleParentComponent { initialState: const initialBlog
                                , render
                                , eval
                                , receiver: const Nothing 
                                , initializer: Just (action Load)
                                , finalizer: Nothing}
  where
    render :: State -> BlogHTML eff
    render state =
      H.div_
      (map renderArticle state.articles)
      

    eval :: Query ~> BlogDSL eff
    eval (Load a) = do
      bs <- liftAff $ getBlogs
      let ids = map (\(Article b) -> {id: b.id, title: b.title, contents: b.contents}) bs
      modify (\s -> s {articles = ids})
      pure a
    

    renderArticle :: {title :: String, contents :: String, id :: ArticleId} -> BlogHTML eff
    renderArticle ar = H.slot
                       (ArticleSlot (ar.id))
                       article
                       (initialArticle ar.id ar.title ar.contents)
                       (const Nothing)


getBlogs :: forall eff. Aff (ajax :: AJAX | eff) (Array Article)
getBlogs = do
  ebs <- runExceptT $ runReaderT getApiBlogs settings
  case ebs of 
    Left err -> pure [Article {title: "ERROR", contents: errorToString err, visible: true, id: -1}]
    Right bs -> pure (convert <$> bs)

convert :: Blog -> Article
convert (Blog {id, title, content}) = initialArticle id title content


