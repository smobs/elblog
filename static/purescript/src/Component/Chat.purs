module Component.Chat where

import Prelude
import WebAPI.Settings
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Servant.Subscriber as Subscribe
import Signal.Channel as Chan
import WebAPI.Subscriber as Sub
import CSS.Transform (offset)
import Control.Category ((<<<))
import Control.Monad (class Monad, pure, bind)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Var (class Updatable)
import Control.Monad.Reader (runReaderT)
import Data.Function ((<<<))
import Data.Generic (gCompare, gShow)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.NaturalTransformation (type (~>))
import Halogen (Component, ComponentDSL, ComponentHTML, EventSource, HalogenEffects, action, eventSource, subscribe)
import Halogen.Component (lifecycleComponent)
import Halogen.Query (set)
import Network.HTTP.Affjax (AJAX, get)
import Servant.Subscriber (Subscriber, SubscriberEff, makeSubscriber)
import Servant.Subscriber.Connection (Config)
import Signal (Signal, runSignal)
import Signal.Channel (Channel, send, channel, CHANNEL)
import WebAPI.Subscriber (getGame)
import WebSocket (WEBSOCKET)

type State = {text :: String}

initial :: State
initial = {text: "Sup?"}

data Query a = Connect a | Disconnect a | UpdateText String a 

type Effects  eff = (ajax :: AJAX, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff)


chat :: forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g, MonadEff (HalogenEffects(Effects eff)) g) => Component State Query g 
chat = lifecycleComponent {render, eval, initializer: Just (action Connect), finalizer: Just (action Disconnect)}
        where 
              render :: State -> ComponentHTML Query
              render {text} = H.text text
              eval :: Query ~> (ComponentDSL State Query g)
              eval (Connect a) = do 
                subscribe chatMessages
                pure a
              eval (Disconnect a) = pure a
              eval (UpdateText t a) = do
                set {text: t}
                pure a

data Action = Update String
            | ReportError 
            | SubscriberLog String
            | Nop

type SubscriberData eff = {
  subscriber :: Subscriber eff Action
, messages :: Signal Action
}

initSubscriber :: forall eff. SubscriberEff (channel :: CHANNEL | eff) (SubscriberData (channel :: CHANNEL | eff))
initSubscriber = do
  ch <- channel Nop
  let
    c :: Config (channel :: CHANNEL | eff) Action
    c = {
        url : "ws://localhost:8080/subscriber"
      , notify : send ch <<< SubscriberLog <<< gShow
      , callback : send ch
      }
  sub <- makeSubscriber c
  let sig = Chan.subscribe ch
  --pongReq <- flip runReaderT settings $ MakeReq.putCounter (CounterAdd 1) -- | Let's play a bit! :-)
  -- closeReq <- flip runReaderT settings $ MakeReq.putCounter (CounterSet 100)
  subs <- flip runReaderT settings $ Sub.getGame (maybe (ReportError) Update)
  let c = Subscribe.getConnection sub
 -- C.setPongRequest pongReq c -- |< Hihi :-)
 -- C.setCloseRequest closeReq c
  Subscribe.deploy subs sub
  pure $ { subscriber : sub, messages : sig }


callback :: forall eff.              
  (Action -> Eff                     
       ( "ref" :: REF        
       , "ws" :: WEBSOCKET   
       , "err" :: EXCEPTION  
       , "channel" :: CHANNEL
       | eff                 
       ) Unit  )                        
  -> Eff                     
       ( "ref" :: REF        
       , "ws" :: WEBSOCKET   
       , "err" :: EXCEPTION  
       , "channel" :: CHANNEL
       | eff                 
       )                     
       Unit
callback eff = do 
    s <- initSubscriber
    runSignal (eff <$> s.messages)

chatMessages ::  forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g) => EventSource Query g
chatMessages = eventSource callback (\a -> case a of
            Update s -> f $ UpdateText s
            Nop -> f $ UpdateText "No op"
            ReportError -> f $ UpdateText "Error" 
            SubscriberLog s -> f $ UpdateText s
        )
        where 
            f x = pure $ action x

