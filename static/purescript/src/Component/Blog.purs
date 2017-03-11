module Component.Blog where

import Prelude
import Halogen
import Model
import Control.Monad.Aff
import Control.Monad.Aff.Class
import Control.Monad.Aff.Free
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Data.Argonaut.Generic.Aeson
import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import Component.Article as Article
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Network.HTTP.Affjax as Ajax
import CSS.Transform (offset)
import Component.Article (article)
import Control.Monad.Except (runExcept)
import DOM.HTML.HTMLTemplateElement (content)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (class Generic, gEq, gCompare)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import Servant.PureScript.Settings (SPSettings_(..))
import Servant.Subscriber.Internal (doCallback)
import ServerAPI (getApiBlogs, SPParams_(..))
import System.BlogRepository (Blog(..))
newtype ArticleSlot = ArticleSlot ArticleId

derive instance articleSlot :: Generic ArticleSlot
instance eqArticleSlot :: Eq ArticleSlot where eq = gEq
instance ordArticleSlot :: Ord ArticleSlot where compare = gCompare

type BlogEffects eff = (ajax :: AJAX | eff)

type State = BlogState
data Query a =
  Load a

type FState g = ParentState BlogState(Article.FState g) Query Article.FQuery g ArticleSlot
type FQuery = Coproduct Query (ChildF ArticleSlot Article.FQuery)
type BlogDSL g = ParentDSL State (Article.FState g) Query Article.FQuery g ArticleSlot
type BlogHTML g = ParentHTML (Article.FState g) Query Article.FQuery g ArticleSlot
-- | The component definition9
blog :: forall a eff. (Functor a, MonadAff (HalogenEffects(BlogEffects eff)) a) =>  Component (FState a) FQuery a
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
getBlogs = do
  ebs <- runExceptT $ runReaderT getApiBlogs settings
  case ebs of 
    Left err -> pure [Article {title: "ERROR", contents: errorToString err, visible: true, id: -1}]
    Right bs -> pure (convert <$> bs)

convert :: Blog -> Article
convert (Blog {id, title, content}) = initialArticle id title content

settings :: SPSettings_ SPParams_
settings =  SPSettings_ { 
                    encodeJson : encodeJson
                  , decodeJson : decodeJson
                  , toURLPiece : gShow
                  , params : SPParams_ {
                      baseURL : "http://localhost:8080/"
                  }}

