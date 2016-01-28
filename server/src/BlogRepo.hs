{-# LANGUAGE DeriveGeneric #-}
module BlogRepo
      (serveBlogs
      , Blog())
 where

import           Control.Monad
import           Data.List        (sortOn)
import           Data.Maybe       (mapMaybe)
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           Text.Read

data Blog = Blog {id :: Int, title :: String, content :: String} deriving (Generic, Eq, Show)

serveBlogs :: FilePath -> IO [Blog]
serveBlogs blogDir = do
           content <- listDir blogDir
           blogDirs <- filterM doesDirectoryExist content
           let numDirs = mapMaybe (\f ->  sequence (f, readMaybe  (takeBaseName f))) blogDirs
           sortOn (\b -> - BlogRepo.id b) <$> traverse readBlog numDirs

     where
        readBlog :: (String ,Int) -> IO Blog
        readBlog (loc, id) = do
           (title, content) <- serveArticle loc
           return $ Blog id title content

        serveArticle :: FilePath -> IO (String, String)
        serveArticle dir = do
                    content <- listDir dir
                    blogFile <- head <$> filterM doesFileExist content
                    contents <- readFile (blogFile)
                    return (takeBaseName blogFile, contents)

listDir :: FilePath -> IO [FilePath]
listDir dir = map (\x -> ( dir ++ "/" ++  x)) <$> getDirectoryContents dir
