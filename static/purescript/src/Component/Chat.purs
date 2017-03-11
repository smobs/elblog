module Component.Chat where

import Prelude
import WebAPI.Settings
import WebAPI
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Servant.Subscriber as Subscribe
import Signal.Channel as Chan
import WebAPI.Subscriber as Sub
import CSS.Transform (offset)
import Control.Category ((<<<))
import Control.Monad (class Monad, pure, bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (error)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Function (const, (<<<))
import Data.Generic (gCompare, gShow)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Monoid (append)
import Data.NaturalTransformation (type (~>))
import Data.Semigroup ((<>))
import Data.String (take)
import Halogen (Component, ComponentDSL, ComponentHTML, EventSource, HalogenEffects, action, eventSource, liftH, modify, subscribe)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Events (onChange)
import Halogen.HTML.Events.Indexed (input_, onClick, onInput, onKeyPress, onSubmit)
import Halogen.HTML.Indexed (button, input, textarea)
import Halogen.Query (get, set)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import Servant.Subscriber (Subscriber, SubscriberEff, makeSubscriber)
import Servant.Subscriber.Connection (Config)
import Servant.Subscriber.Internal (doCallback)
import Signal (Signal, runSignal)
import Signal.Channel (Channel, send, channel, CHANNEL)
import WebAPI.Subscriber (getGame)
import WebSocket (WEBSOCKET)

type State = {cur :: String , text :: Array String}

initial :: State
initial = {cur: "", text: []}

data Query a = Connect a | Disconnect a | UpdateText String a | SendMessage a | UpdateCurrent String a

type Effects  eff = (ajax :: AJAX, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET | eff)

onlyForKey :: forall props g ev.                       
  (Functor g) => Number                      
                   -> ({ "keyCode" :: Number  
                       | props                  
                       }                      
                       -> g (Maybe ev)     
                      )                       
                      -> { "keyCode" :: Number
                         | props                
                         }
                         -> g (Maybe ev)
onlyForKey i input  = (\ (a@{keyCode}) -> (if keyCode == 13.0 then id else const Nothing) <$> input a)

chat :: forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g, MonadAff (HalogenEffects(Effects eff)) g) => Component State Query g 
chat = lifecycleComponent {render, eval, initializer: Just (action Connect), finalizer: Just (action Disconnect)}
        where 
              render :: State -> ComponentHTML Query
              render {text, cur} = H.div_ $ 
                    append [ H.input [ P.value cur
                                     , E.onValueInput (E.input UpdateCurrent)
                                     , E.onKeyPress  (onlyForKey 13.0 (E.input_ SendMessage))
                                     ]
                            , H.button [E.onClick (E.input_ SendMessage)] [H.text "Send"]]
                        $ (\t -> H.div_ [H.text t]) <$> text

              eval :: Query ~> (ComponentDSL State Query g)
              eval (Connect a) = do 
                subscribe chatMessages
                pure a
              eval (Disconnect a) = pure a
              eval (UpdateText t a) = do
                modify (\s ->  s {text= append [t] s.text})
                pure a
              eval (SendMessage a) = do
                st <- get
                merr <- liftH <<< liftAff $ sendMessage st.cur
                case merr of
                    Just err -> do modify (\s ->  s {text= append [err] s.text})
                                   pure a
                    Nothing -> do set {cur: "", text: st.text}
                                  pure a
              eval (UpdateCurrent s a) = do
                modify (\st -> st {cur = s})
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
  let conn = Subscribe.getConnection sub
 -- C.setPongRequest pongReq conn -- |< Hihi :-)
 -- C.setCloseRequest closeReq conn
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


sendMessage :: forall eff. String -> Aff (ajax :: AJAX | eff) (Maybe String)
sendMessage s = do
    ebs <- runExceptT $ runReaderT (postGame s) settings
    pure $ case ebs of
        Left err -> Just $ errorToString err
        _ -> Nothing
