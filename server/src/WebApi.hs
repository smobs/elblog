{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module WebApi where

import Servant
import Servant.Subscriber.Subscribable
import System.BlogRepository
import PSApp
import Servant.HTML.Blaze

type BlogApi = "api" :> "blogs" :> Get '[JSON] [Blog]

type HomeApi = Get '[HTML] PSApp

blogAPI :: Proxy BlogApi
blogAPI = Proxy

type SiteApi = HomeApi :<|> BlogApi :<|> "static" :> Raw

type GameApi = "game" :> Subscribable :> Get '[JSON] [String]

siteAPI :: Proxy SiteApi
siteAPI = Proxy