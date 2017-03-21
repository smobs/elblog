{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module WebApi where

import Servant
import Servant.Subscriber.Subscribable
import System.BlogRepository
import PSApp
import Servant.HTML.Blaze
import GHC.Generics
import Data.Text (Text)
import Data.Wizard.View
import Data.Wizard.Command

data ChatMessage = ChatMessage {userName :: Text, messageBody :: Text } deriving (Generic, Eq, Show)

data AuthToken = AuthToken Text deriving (Generic, Eq, Show)

type BlogApi = "api" :> "blogs" :> Get '[JSON] [Blog]
type HomeApi = Get '[HTML] PSApp
type ChatApi = "chat" :> (Subscribable :> Get '[JSON] [ChatMessage]
                :<|> ReqBody '[JSON] Text :> Post '[JSON] ())

type GameApi = "game" :> (Subscribable :> Get '[JSON] GameView
                       :<|> "input" :> ReqBody '[JSON] GameCommand :> Post '[JSON] ())

type AppApi = BlogApi :<|>  (Header "AuthToken" AuthToken :> (ChatApi :<|> GameApi))

appAPI :: Proxy AppApi
appAPI = Proxy

type SiteApi = HomeApi :<|> AppApi :<|> "static" :> Raw


siteAPI :: Proxy SiteApi
siteAPI = Proxy