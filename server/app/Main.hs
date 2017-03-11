{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Maybe               (fromMaybe)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           PSApp
import           WebApi
import           Servant
import           Servant.HTML.Blaze
import           System.BlogRepository
import           System.Environment
import           Servant.Subscriber.Subscribable

type ServerData = String

type GameHandler = ReaderT ServerData Handler 

transformGameHandler :: ServerData -> (GameHandler :~> Handler)
transformGameHandler sd = runReaderTNat sd

main :: IO ()
main = do
  p <- port
  run p (app "Wonderbar")

app :: ServerData -> Application
app sd = serve siteAPI (server sd)

instance ToJSON Blog

gameHandler :: ServerT GameApi GameHandler
gameHandler = pure "Hello tokens"

apiHandler :: ServerData -> Server AppApi
apiHandler sd = blogHandler
                 :<|> (enter (transformGameHandler sd) gameHandler)

server ::  ServerData -> Server SiteApi
server sd = return (PSApp "static")
         :<|> (apiHandler sd)
         :<|> serveDirectory "static/dist/"

blogHandler :: Server BlogApi
blogHandler  = liftIO $ serveBlogs "blog"



port :: IO Int
port = do
  env <- getEnvironment
  let port = fromMaybe "8080" $ lookup "PORT" env
  return (read port)
