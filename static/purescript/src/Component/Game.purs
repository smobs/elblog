module Component.Game where 

import WebAPI.Settings
import WebAPI.Subscriber as Sub
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Chat.ServerTypes
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (clearInterval, setInterval, IntervalId, TIMER)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Component.LoginWidget as Login
import Data.Generic
import Data.Boolean (otherwise) 
import Data.Eq ((==))
import Data.Int (ceil)
import Data.Maybe (Maybe(..), maybe)  
import Data.Semigroup ((<>))
import Graphics.Canvas (CANVAS)
import Halogen (ComponentDSL, ComponentHTML, Component, HalogenEffects, action, component, liftH, get, modify, subscribe, eventSource ,eventSource_, EventSource)
import Halogen.HTML.Properties (pixels)

import Prelude 
import Halogen.Query (get, set)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import Servant.Subscriber (Subscriber, SubscriberEff, makeSubscriber)
import Servant.Subscriber as Subscribe
import Servant.Subscriber.Connection (Config)
import Servant.Subscriber.Internal (doCallback)
import Signal (Signal, runSignal)
import Signal.Channel (Channel, send, channel, CHANNEL)
import Signal.Channel as Chan
import WebSocket (WEBSOCKET)
import DOM(DOM)
import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location

type State = {i :: Int, auth :: Maybe AuthToken, login :: String}

initial :: State
initial = {i: 0, auth: Nothing, login: ""}

type KeyCode = Number
data Query a = NewGame a | Input KeyCode a | UpdateLogin String a | SetAuth a | NoOp a

type Effects eff = (ajax :: AJAX, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET, canvas :: CANVAS , console :: CONSOLE | eff)

game :: forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g, MonadEff (HalogenEffects(Effects eff)) g) => Component State Query g 
game = component {render, eval}
            where
              render :: State -> ComponentHTML Query
              render {auth: Just _} = H.div [] [ H.canvas [ P.id_ canvasName
                                             , E.onKeyDown  (E.input (\ {keyCode} -> Input keyCode))
                                             , P.tabIndex 0
                                             , P.height $ pixels $ ceil canvasSize.h
                                             , P.width
                                               $ pixels
                                               $ ceil canvasSize.w ]]
              render _ = Login.render UpdateLogin SetAuth
              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) =
                pure a
              eval (Input keyCode a) =
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
                pure a
              eval (NoOp a) = pure a

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

data Action = Update (Array ChatMessage)
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
  -- closeReq <- flip runReaderT settings $ MakeReq.putCounter (CounterSet 100)
  subs <- flip runReaderT settings $ Sub.getChat (maybe (ReportError) Update) a
  let conn = Subscribe.getConnection sub
 -- C.setPongRequest pongReq conn -- |< Hihi :-)
 -- C.setCloseRequest closeReq conn
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
            Update s -> f $ NoOp
            Nop -> f $ NoOp
            ReportError -> f $ NoOp
            SubscriberLog s -> f $ NoOp
        )
        where 
            f x = pure $ action x
