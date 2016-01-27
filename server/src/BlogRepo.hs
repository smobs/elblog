{-# LANGUAGE DeriveGeneric #-}
module BlogRepo
      (serveBlogs
      , Blog())
 where

import           Control.Monad
import           Data.Maybe       (mapMaybe)
import           GHC.Generics
import           System.Directory
import           Text.Read

data Blog = Blog {id :: Int, title :: String, content :: String} deriving (Generic, Eq, Show)

serveBlogs :: IO [Blog]
serveBlogs = do
           content <- listDir "blog"
           blogDirs <- filterM  (\(_, f) ->  doesDirectoryExist f) content
           let numDirs =   mapMaybe (\(i, f) ->  sequence (f, readMaybe i)) blogDirs
           traverse readBlog numDirs

     where
        readBlog :: (String ,Int) -> IO Blog
        readBlog (loc, id) = do
           (title, content) <- serveArticle loc
           return $ Blog id title content


serveArticle :: FilePath -> IO (String, String)
serveArticle dir = do
             content <- map snd <$> listDir dir
             blogFile <- head <$> filterM doesFileExist content
             contents <- readFile (blogFile)
             return (blogFile, contents)


listDir :: FilePath -> IO [ (FilePath, FilePath)]
listDir dir = map (\x -> (x,  dir ++ "/" ++  x)) <$> getDirectoryContents dir
