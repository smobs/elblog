{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           ElmApp
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
main :: IO ()
main = run 8081 app

app :: Application
app = serve siteAPI server

type Blog = String

type BlogApi = "api" :> "blog" :> Get '[JSON] Blog

type HomeApi = Get '[HTML] ElmApp

type SiteApi = HomeApi :<|> BlogApi :<|> "elm" :> Raw

server :: Server SiteApi
server = return (ElmApp "elm")
         :<|> return "Hello world"
         :<|>  serveDirectory "elm"


siteAPI :: Proxy SiteApi
siteAPI = Proxy
