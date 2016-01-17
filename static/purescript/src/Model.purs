module Model where

import Prelude

type ArticleId = Int

type Blog = {articles :: Array ArticleId}

type Article = {contents :: String, visible :: Boolean, title :: String, id :: ArticleId}

initialBlog :: Blog
initialBlog = {articles : []}

initialArticle :: ArticleId -> Article
initialArticle i = let id = show i in
  {title : "", visible : false, contents : "", id : i}
