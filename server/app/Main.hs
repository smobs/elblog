{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           BlogRepo
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Maybe               (fromMaybe)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           PSApp
import           Servant
import           Servant.HTML.Blaze
import           System.Environment

main :: IO ()
main = do
  p <- port
  run p app

app :: Application
app = serve siteAPI server

instance ToJSON Blog

type BlogApi = "api" :> "blogs" :> Get '[JSON] [Blog]

type HomeApi = Get '[HTML] PSApp

type SiteApi = HomeApi :<|> BlogApi :<|> "static" :> Raw

server ::  Server SiteApi
server = return (PSApp "static")
         :<|> blogHandler
         :<|> serveDirectory "static/dist/"

blogHandler :: Server BlogApi
blogHandler  = liftIO $ serveBlogs "blog"

siteAPI :: Proxy SiteApi
siteAPI = Proxy

port :: IO Int
port = do
  env <- getEnvironment
  let port = fromMaybe "8080" $ lookup "PORT" env
  return (read port)
