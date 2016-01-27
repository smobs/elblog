{-# LANGUAGE DeriveGeneric #-}
module BlogRepo
      (serveBlogs
      , Blog())
 where

import           GHC.Generics
data Blog = Blog {id :: Int, title :: String, content :: String} deriving (Generic, Eq, Show)

serveBlogs :: IO [Blog]
serveBlogs = do
           return []
