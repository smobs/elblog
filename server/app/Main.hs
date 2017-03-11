{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Logger               (runStderrLoggingT)
import           Data.Aeson
import           Data.Maybe               (fromMaybe)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           PSApp
import           WebApi
import           Control.Concurrent.STM
import           Servant
import           Servant.HTML.Blaze
import           System.BlogRepository
import           System.Environment
import           Servant.Subscriber.Subscribable
import           Servant.Subscriber

type ServerData = String

type GameHandler = ReaderT ServerData Handler 

transformGameHandler :: ServerData -> (GameHandler :~> Handler)
transformGameHandler sd = runReaderTNat sd

main :: IO ()
main = do
  p <- port
  cd <- atomically (makeSubscriber "subscriber" runStderrLoggingT)
  run p $ app "Wonderbar" cd

app :: ServerData -> Subscriber SiteApi -> Application
app sd sub = serveSubscriber sub (server sd)

instance ToJSON Blog

postGameHandler :: String -> ReaderT ServerData Handler ()
postGameHandler s = pure ()

gameHandler :: ServerT GameApi GameHandler
gameHandler = pure "Hello tokens"
              :<|> postGameHandler
 
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
