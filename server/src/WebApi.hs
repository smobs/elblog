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
type GameApi = "game" :> (Subscribable :> Get '[JSON] [String]
                :<|> ReqBody '[JSON] String :> Post '[JSON] ())

type AppApi = BlogApi :<|> GameApi

appAPI :: Proxy AppApi
appAPI = Proxy

type SiteApi = HomeApi :<|> AppApi :<|> "static" :> Raw


siteAPI :: Proxy SiteApi
siteAPI = Proxy