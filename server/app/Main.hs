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

data Blog = Blog {id :: Int, title :: String, content :: String} deriving (Generic, Eq, Show)

instance ToJSON Blog

type BlogApi = "api" :> "blogs" :> Get '[JSON] [Blog]

type HomeApi = Get '[HTML] PSApp

type SiteApi = HomeApi :<|> BlogApi :<|> "static" :> Raw

server ::  Server SiteApi
server = return (PSApp "static")
         :<|> blogHandler [1,2,3]
         :<|> serveDirectory "static/dist/"


blogHandler ids = return $
 map
 (\i -> Blog i ("Blog " ++ show i) ("Content"))
 ids

siteAPI :: Proxy SiteApi
siteAPI = Proxy

port :: IO Int
port = do
  env <- getEnvironment
  let port = fromMaybe "8080" $ lookup "PORT" env
  return (read port)
