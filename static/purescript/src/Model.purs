module Model where

import Prelude

type Blog = {articles :: Array Article}

type Article = String

initialBlog :: Blog
initialBlog = {articles : ["Hello world", "New world", "End of the world"]}
