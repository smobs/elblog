module Component.Chat where

import Prelude
import WebAPI.Settings
import WebAPI
import Chat.ServerTypes
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Servant.Subscriber as Subscribe
import Signal.Channel as Chan
import WebAPI.Subscriber as Sub
import Control.Category ((<<<))
import Control.Monad (class Monad, pure, bind)
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
import DOM(DOM)
import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location

type State = {cur :: String , text :: Array ChatMessage, sub :: Boolean}

initial :: State
initial = {cur: "", text: [], sub: false}

data Query a = Connect a | Disconnect a | UpdateText (Array ChatMessage) a | SendMessage a | UpdateCurrent String a

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
              render {text, cur, sub} = H.div_ $ 
                    append (if not sub 
                        then []
                        else [ H.input [ P.value cur
                                       , E.onValueInput (E.input UpdateCurrent)
                                       , E.onKeyPress  (onlyForKey 13.0 (E.input_ SendMessage))
                                       ]
                            , H.button [E.onClick (E.input_ SendMessage)] [H.text "Send"]])
                        $ (\(ChatMessage t) -> H.div_ [H.text (t.userName <> ": " <> t.messageBody)]) <$> text

              eval :: Query ~> (ComponentDSL State Query g)
              eval (Connect a) = do 
                sub <- liftH $ liftEff initSubscriber 
                subscribe (chatMessages sub.messages)
                modify (\s -> s {sub = true})
                pure a
              eval (Disconnect a) = pure a
              eval (UpdateText t a) = do
                modify (\s ->  s {text= t})
                pure a
              eval (SendMessage a) = do
                st <- get
                merr <- liftH <<< liftAff $ sendMessage st.cur
                case merr of
                    Just err -> do modify (\s ->  s {text= append [systemMessage err] s.text})
                                   pure a
                    Nothing -> do modify (\s -> s {cur = ""})
                                  pure a
              eval (UpdateCurrent s a) = do
                modify (\st -> st {cur = s})
                pure a

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

initSubscriber :: forall eff. SubscriberEff (channel :: CHANNEL, dom ::DOM  | eff) (SubscriberData (channel :: CHANNEL, dom ::DOM | eff))
initSubscriber = do
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
  subs <- flip runReaderT settings $ Sub.getGame (maybe (ReportError) Update)
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

systemMessage :: String -> ChatMessage
systemMessage s = ChatMessage {userName: "System", messageBody: s}

chatMessages ::  forall g eff. (Monad g, Affable (HalogenEffects(Effects eff)) g) => Signal Action ->  EventSource Query g
chatMessages sig = eventSource (callback sig) (\a -> case a of
            Update s -> f $ UpdateText s
            Nop -> f $ UpdateText [systemMessage "No op"]
            ReportError -> f $ UpdateText [systemMessage "Error"]
            SubscriberLog s -> f $ UpdateText [systemMessage s]
        )
        where 
            f x = pure $ action x


sendMessage :: forall eff. String -> Aff (ajax :: AJAX | eff) (Maybe String)
sendMessage s = do
    ebs <- runExceptT $ runReaderT (postGame s) settings
    pure $ case ebs of
        Left err -> Just $ errorToString err
        _ -> Nothing
