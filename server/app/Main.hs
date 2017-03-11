{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Logger               (runStderrLoggingT)
import           Data.Aeson
import           Data.Maybe               (fromMaybe)
import           Data.IORef
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

data ServerData = ServerData {
  messageRef :: IORef String
}

type GameHandler = (ReaderT ServerData Handler) 

transformGameHandler :: ServerData -> (GameHandler :~> Handler)
transformGameHandler sd = runReaderTNat sd

main :: IO ()
main = do
  p <- port
  cd <- atomically (makeSubscriber "subscriber" runStderrLoggingT)
  ref <- newIORef "Wonderbar"
  run p $ app (ServerData ref) cd

app :: ServerData -> Subscriber SiteApi -> Application
app sd sub = serveSubscriber sub (server sd)

instance ToJSON Blog

postGameHandler :: String -> ReaderT ServerData Handler ()
postGameHandler s = pure ()

getGameHandler :: ReaderT ServerData Handler String
getGameHandler = do 
  ref <- ask
  liftIO (readIORef (messageRef ref))
              

gameHandler :: ServerT GameApi GameHandler
gameHandler = getGameHandler :<|> postGameHandler
 
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
