{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Data.Maybe               (fromMaybe)
import           ElmApp
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
import           System.Environment

main :: IO ()
main = do
  p <- port
  run p app

app :: Application
app = serve siteAPI server


type Blog = String

type BlogApi = "api" :> "blog" :> Get '[JSON] Blog

type HomeApi = Get '[HTML] ElmApp

type SiteApi = HomeApi :<|> BlogApi :<|> "static" :> Raw

server ::  Server SiteApi
server = return (ElmApp "static/dist")
         :<|> return "Hello world"
         :<|>  serveDirectory "static"


siteAPI :: Proxy SiteApi
siteAPI = Proxy

port :: IO Int
port = do
  env <- getEnvironment
  let port = fromMaybe "8080" $ lookup "PORT" env
  return (read port)
