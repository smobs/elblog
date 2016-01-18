{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Data.Aeson
import           Data.Maybe               (fromMaybe)
import           GHC.Generics
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

data Blog = Blog {title :: String, content :: String} deriving (Generic, Eq, Show)

instance ToJSON Blog

type BlogApi = "api" :> "blogs" :> Get '[JSON] [Int]
     :<|> "api" :> "blog" :> Capture "blogId"  Int :> Get '[JSON] Blog

type HomeApi = Get '[HTML] PSApp

type SiteApi = HomeApi :<|> BlogApi :<|> "static" :> Raw

server ::  Server SiteApi
server = return (PSApp "static")
         :<|> (return [1,2,3]
         :<|> blogHandler)
         :<|> serveDirectory "static/dist/"

blogHandler id = let i = show id
                 in return $ Blog ("Blog " ++ i) ("Content " ++ i)

siteAPI :: Proxy SiteApi
siteAPI = Proxy

port :: IO Int
port = do
  env <- getEnvironment
  let port = fromMaybe "8080" $ lookup "PORT" env
  return (read port)
