module Component.Game where 

import WebAPI
import WebAPI.Settings
import WebAPI.MakeRequests as MakeReq
import Chat.ServerTypes
import Data.Wizard.View
import Data.Generic
import Data.Int
import Prelude
import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location
import Graphics.Game
import Component.LoginWidget as Login
import Data.Wizard.Command as Com
import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Servant.Subscriber as Subscribe
import Signal.Channel as Chan
import WebAPI.Subscriber as Sub
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (clearInterval, setInterval, IntervalId, TIMER)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import DOM (DOM)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Int (ceil)
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Wizard.Command (GameCommand(..))
import Graphics.Canvas (CANVAS)
import Halogen (ComponentDSL, ComponentHTML, Component, HalogenEffects, action, liftH, get, modify, subscribe, eventSource, eventSource_, EventSource)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Properties (pixels)
import Halogen.Query (get, set)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import Servant.Subscriber (Subscriber, SubscriberEff, makeSubscriber)
import Servant.Subscriber.Connection (Config, setCloseRequest)
import Servant.Subscriber.Internal (doCallback)
import Signal (Signal, runSignal)
import Signal.Channel (Channel, send, channel, CHANNEL)
import WebSocket (WEBSOCKET)

type State = {auth :: Maybe AuthToken, login :: String}

initial :: State
initial = { auth: Nothing, login: ""}

type KeyCode = Number
data Query a = NewGame a | Input KeyCode Boolean a | UpdateLogin String a | SetAuth a | UpdateGame GameView a | NoOp a | Close a

type Effects eff = (ajax :: AJAX, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET, canvas :: CANVAS , console :: CONSOLE | eff)

game :: forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g, MonadAff (HalogenEffects(Effects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Nothing, finalizer: Just $ action Close}
            where
              render :: State -> ComponentHTML Query
              render {auth: Just _} = H.div [] [ H.canvas [ P.id_ canvasName
                                             , E.onKeyDown  (E.input (\ {keyCode} -> Input keyCode true))
                                             , E.onKeyUp (E.input (\ {keyCode} -> Input keyCode false ))
                                             , P.tabIndex 0
                                             , P.height $ pixels $ ceil canvasSize.h
                                             , P.width
                                               $ pixels
                                               $ ceil canvasSize.w ]]
              render _ = Login.render UpdateLogin SetAuth
              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) =
                pure a
              eval (Input keyCode down a) =do
                st <- get
                case do 
                    au <- st.auth 
                    com <- lookupControls keyCode down
                    pure (Tuple au com) of
                  Nothing -> pure a
                  Just (Tuple t com) -> do 
                       merr <- liftH <<< liftAff $ sendCommand com t
                       pure a
              eval (UpdateLogin l a) = do
                modify (\st -> st {login = l})
                pure a
              eval (SetAuth a) = do
                st <- get
                let t = (AuthToken st.login)
                set $ st {auth = Just t}
                sub <- liftH $ liftEff $ initSubscriber t 
                subscribe (gameMessages sub.messages)
                liftH <<< liftAff $ sendCommand (Com.Configuration (Com.AddPlayer)) t
                pure a
              eval (NoOp a) = pure a
              eval (UpdateGame g a) = do
                liftH <<< liftEff $ renderGame canvasSize.w canvasSize.h canvasName g
                pure a
              eval (Close a) = do
                st <- get
                case st.auth of
                    Nothing -> pure a
                    Just  t -> do 
                        liftH <<< liftAff $ sendCommand (Com.Configuration (Com.RemovePlayer)) t
                        pure a

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 600.0, w: 1200.0}

data Action = Update GameView
            | ReportError 
            | SubscriberLog String
            | Nop

type SubscriberData eff = {
  subscriber :: Subscriber eff Action
, messages :: Signal Action
}
websocketUrl :: forall eff. Eff (dom :: DOM | eff) String
websocketUrl = do
  l <- location =<< window
  h <- host l
  hname <- hostname l
  let p = case hname of
            "localhost" -> "ws:"
            _ -> "wss:"
  pure $ p <> "//"<> h

initSubscriber :: forall eff. AuthToken -> SubscriberEff (channel :: CHANNEL, dom ::DOM  | eff) (SubscriberData (channel :: CHANNEL, dom ::DOM | eff))
initSubscriber a = do
  ch <- channel Nop
  url <- websocketUrl
  let
    c :: Config (channel :: CHANNEL, dom ::DOM | eff) Action
    c = {
        url : url <> "/subscriber"
      , notify : send ch <<< SubscriberLog <<< gShow
      , callback : send ch
      }
  sub <- makeSubscriber c
  let sig = Chan.subscribe ch
  --pongReq <- flip runReaderT settings $ MakeReq.putCounter (CounterAdd 1) -- | Let's play a bit! :-)
  closeReq <- flip runReaderT settings $ MakeReq.postGameInput a (Com.Configuration (Com.RemovePlayer))
  subs <- flip runReaderT settings $ Sub.getGame (maybe (ReportError) Update) a
  let conn = Subscribe.getConnection sub
 -- C.setPongRequest pongReq conn -- |< Hihi :-)
  setCloseRequest closeReq conn
  Subscribe.deploy subs sub
  pure $ { subscriber : sub, messages : sig }


callback :: forall eff.    
  Signal Action ->          
  (Action -> Eff                     
       ( "ref" :: REF        
       , "ws" :: WEBSOCKET   
       , "err" :: EXCEPTION 
       , "channel" :: CHANNEL
       , dom ::DOM
       | eff                 
       ) Unit  )                        
  -> Eff                     
       ( "ref" :: REF        
       , "ws" :: WEBSOCKET   
       , "err" :: EXCEPTION  
       , "channel" :: CHANNEL
       , dom ::DOM
       | eff                 
       )                     
       Unit
callback sig eff = do 
    runSignal (eff <$> sig)

gameMessages ::  forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g) => Signal Action ->  EventSource Query g
gameMessages sig = eventSource (callback sig) (\a -> case a of
            Update s -> f $ UpdateGame s
            Nop -> f $ NoOp
            ReportError -> f $ NoOp
            SubscriberLog s -> f $ NoOp
        )
        where 
            f x = pure $ action x

sendCommand :: forall eff. GameCommand -> AuthToken -> Aff (ajax :: AJAX | eff) (Maybe String)
sendCommand s a = do
    ebs <- runExceptT $ runReaderT (postGameInput a s) settings
    pure $ case ebs of
        Left err -> Just $ errorToString err
        _ -> Nothing


lookupControls :: KeyCode -> Boolean -> Maybe GameCommand
lookupControls 37.0 d = Just $ (if d then Move else StopMove) Com.Left
lookupControls 38.0 d = Just $ (if d then Move else StopMove) Com.Up
lookupControls 39.0 d = Just $ (if d then Move else StopMove) Com.Right
lookupControls 40.0 d = Just $ (if d then Move else StopMove) Com.Down
lookupControls _ _ = Nothing

