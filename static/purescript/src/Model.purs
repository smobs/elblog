module Model where

import Prelude

type ArticleId = Int

type Blog = {articles :: Array ArticleId}

type Article = {contents :: String, visible :: Boolean, title :: String, id :: ArticleId}

initialBlog :: Blog
initialBlog = {articles : [1, 2, 13]}

initialArticle :: ArticleId -> Article
initialArticle i = let id = show i in
  {title : "Title" ++ id, visible : false, contents : "Hello " ++ id, id : i}
