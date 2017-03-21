{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Data.Wizard
import Data.Wizard.View
import Data.Wizard.Command

import GHC.IO.Encoding
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
import Servant.PureScript (jsonParseUrlPiece, jsonParseHeader)

import Data.Text(Text)

data ServerData = ServerData { messageRef :: IORef [ChatMessage]
                             , gameRef :: IORef GameState
                             , subscriber :: Subscriber SiteApi}

type ChatHandler = (ReaderT ServerData Handler) 

transformGameHandler :: ServerData ->  (ChatHandler :~> Handler)
transformGameHandler sd = runReaderTNat sd

main :: IO ()
main = do
  setLocaleEncoding utf8
  p <- port
  cd <- atomically (makeSubscriber "subscriber" runStderrLoggingT)
  mref <- newIORef [ChatMessage "System" "First post!!!1!1!"]
  gref <- newIORef $ GameState 1
  run p $ app (ServerData mref gref cd) cd

app :: ServerData -> Subscriber SiteApi -> Application
app sd sub = serveSubscriber sub (server sd)

instance ToJSON Blog
instance ToJSON ChatMessage

instance ToJSON Shape
instance ToJSON GameView

instance FromJSON AuthToken
instance FromJSON GameCommand
instance FromJSON Direction


instance FromHttpApiData AuthToken where
  parseUrlPiece = jsonParseUrlPiece
  parseHeader   = jsonParseHeader

postChatHandler :: Text -> Text -> ReaderT ServerData Handler ()
postChatHandler n s = do
  let d = ChatMessage n s
  r <- liftIO . flip atomicModifyIORef' (doAction d) =<< (messageRef <$> ask)

  subscriber' <- (subscriber <$> ask)
  let link :: Proxy ("chat" :>  Get '[JSON] [ChatMessage])
      link = Proxy
  liftIO . atomically $ notify subscriber' ModifyEvent link id
  pure ()
  where
    doAction s ms = (take 10 $ s : ms, take 10 $ s : ms)


getChatHandler :: ReaderT ServerData Handler [ChatMessage]
getChatHandler = do 
  ref <- ask
  liftIO (readIORef (messageRef ref))

getGameHandler :: ReaderT ServerData Handler GameView
getGameHandler = do 
  ref <- ask
  liftIO (stateToGameView <$> readIORef (gameRef ref))

postGameInputHandler :: Text -> GameCommand -> ReaderT ServerData Handler ()
postGameInputHandler n kc = do
  r <- liftIO . flip atomicModifyIORef' (doAction kc) =<< (gameRef <$> ask)

  subscriber' <- (subscriber <$> ask)
  let link :: Proxy ("game" :> Get '[JSON] GameView)
      link = Proxy
  liftIO . atomically $ notify subscriber' ModifyEvent link id
  pure ()
  where
    doAction i g = (updateGame i g, updateGame i g)


chatHandler :: AuthToken -> ServerT ChatApi ChatHandler
chatHandler (AuthToken t) = getChatHandler :<|> (postChatHandler t)

gameHandler :: AuthToken -> ServerT GameApi ChatHandler
gameHandler (AuthToken t) = getGameHandler :<|> (postGameInputHandler t)

userMissingError :: Server (ChatApi :<|> GameApi)
userMissingError = let e = throwError $ err401 { errBody = "You have to provide a username!" }
                   in (e :<|> (\s -> e)) :<|> (e :<|> (\s -> e))


apiHandler :: ServerData -> Server AppApi
apiHandler sd = blogHandler
                 :<|> (\auth -> case auth of
                                Just a -> enter (transformGameHandler sd) $ (chatHandler a :<|> gameHandler a)
                                Nothing -> userMissingError)

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
