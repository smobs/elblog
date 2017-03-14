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

data ServerData = ServerData { messageRef :: IORef [String]
                             , subscriber :: Subscriber SiteApi}

type GameHandler = (ReaderT ServerData Handler) 

transformGameHandler :: ServerData -> (GameHandler :~> Handler)
transformGameHandler sd = runReaderTNat sd

main :: IO ()
main = do
  p <- port
  cd <- atomically (makeSubscriber "subscriber" runStderrLoggingT)
  ref <- newIORef ["First post!!!1!1!"]
  run p $ app (ServerData ref cd) cd

app :: ServerData -> Subscriber SiteApi -> Application
app sd sub = serveSubscriber sub (server sd)

instance ToJSON Blog

postGameHandler :: String -> ReaderT ServerData Handler ()
postGameHandler s = do
  r <- liftIO . flip atomicModifyIORef' (doAction s) =<< (messageRef <$> ask)

  subscriber' <- (subscriber <$> ask)
  let link :: Proxy ("game" :>  Get '[JSON] [String])
      link = Proxy
  liftIO . atomically $ notify subscriber' ModifyEvent link id
  pure ()
  where
    doAction s ms = (take 10 $ s : ms, take 10 $ s : ms)


getGameHandler :: ReaderT ServerData Handler [String]
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
