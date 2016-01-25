module Model where

import Prelude

import Data.Foreign
import Data.Foreign.Class

type ArticleId = Int

type Blog = {articles :: Array {title :: String, contents :: String, id :: ArticleId}}

data Article = Article {contents :: String, visible :: Boolean, title :: String, id :: ArticleId}

initialBlog :: Blog
initialBlog = {articles : []}

initialArticle :: ArticleId -> String -> String -> Article
initialArticle i t c =
  Article {title : t, visible : false, contents : c, id : i}


instance articleIsForeign :: IsForeign Article where
  read value = do
    id <- readProp "id" value
    title <- readProp "title" value
    contents <- readProp "content" value
    return $ Article {contents: contents, visible: false, title: title, id: id}
