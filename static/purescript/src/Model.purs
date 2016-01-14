module Model where

import Prelude

type ArticleId = Int

type Blog = {articles :: Array ArticleId}

type Article = String

initialBlog :: Blog
initialBlog = {articles : [1, 2, 13]}

initialArticle :: ArticleId -> Article
initialArticle i = "Hello" ++ (show i) 
