module Component.Chat where

import Prelude
import WebAPI.Settings
import WebAPI
import Chat.ServerTypes
import Component.LoginWidget as Login
import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Servant.Subscriber as Subscribe
import Signal.Channel as Chan
import WebAPI.Subscriber as Sub
import Control.Category ((<<<))
import Control.Monad (class Monad, pure, bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar(AVAR)
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
import Halogen (Component, ComponentDSL, ComponentHTML, EventSource, SubscribeStatus(Listening), action, eventSource, modify, subscribe, get, put)
import Halogen.Component (lifecycleComponent)
import Halogen.HTML.Events (onChange)
import Halogen.HTML.Events (input_, onClick, onInput, onKeyPress, onSubmit)
import Halogen.HTML (button, input, textarea)
import Network.HTTP.Affjax (AJAX)
import Halogen.Component
import Servant.PureScript.Affjax (errorToString)
import Servant.Subscriber (Subscriber, SubscriberEff, makeSubscriber)
import Servant.Subscriber.Connection (Config)
import Servant.Subscriber.Internal (doCallback)
import Signal (Signal, runSignal)
import Signal.Channel (Channel, send, channel, CHANNEL)
import WebSocket (WEBSOCKET)
import DOM(DOM)
import DOM.HTML
import DOM.HTML.Window
import DOM.HTML.Location

type State = {cur :: String , text :: Array ChatMessage, sub :: Boolean, auth :: Maybe AuthToken, login :: String}

initial :: State
initial = {cur: "", text: [], sub: false, auth: Nothing, login: ""}

data Query a = Connect a | Disconnect a | UpdateText (Array ChatMessage) a | SendMessage a | UpdateCurrent String a | SetAuth a | UpdateLogin String a

type Effects  eff = (ajax :: AJAX, channel :: CHANNEL, ref :: REF, ws :: WEBSOCKET, avar :: AVAR, dom :: DOM, err :: EXCEPTION | eff)

chat :: forall eff. Component H.HTML Query Unit Void (Aff (Effects eff)) 
chat = lifecycleComponent spec
        where 
              spec :: LifecycleComponentSpec H.HTML State Query Unit Void (Aff (Effects eff))
              spec = { initialState: const initial 
                     , render
                     , eval
                     , receiver: const Nothing
                     , initializer: Just (action Connect)
                     , finalizer: Just (action Disconnect)}
              render :: State -> ComponentHTML Query
              render {text, cur, sub, auth: Just _ } = H.div_ $ 
                    append (if not sub 
                        then []
                        else [ H.input [ P.value cur
                                       , E.onValueInput (E.input UpdateCurrent)
                                       , Login.onSpecificKeyPress "Enter" ((E.input_ SendMessage))
                                       ]
                            , H.button [E.onClick (E.input_ SendMessage)] [H.text "Send"]])
                        $ (\(ChatMessage t) -> H.div_ [H.text (t.userName <> ": " <> t.messageBody)]) <$> text
              render _ = Login.render UpdateLogin SetAuth
              eval :: Query ~> ComponentDSL State Query Void (Aff (Effects eff))
              eval (Connect a) = do
                st <- get
                case st.auth of
                  Nothing -> pure a
                  Just token -> do
                    sub <- liftEff (initSubscriber token)
                    subscribe (chatMessages sub.messages)
                    modify (\s -> s {sub = true})    
                    pure a
              eval (Disconnect a) = pure a
              eval (UpdateText t a) = do
                modify (\s ->  s {text= t})
                pure a
              eval (SendMessage a) = do
                st <- get
                case st.auth of
                  Nothing -> pure a
                  Just t -> do 
                       merr <- liftAff $ sendMessage st.cur t
                       case merr of
                           Just err -> do modify (\s ->  s {text= append [systemMessage err] s.text})
                                          pure a
                           Nothing -> do modify (\s -> s {cur = ""})
                                         pure a
              eval (UpdateCurrent s a) = do
                modify (\st -> st {cur = s})
                pure a
              eval (UpdateLogin l a) = do
                modify (\st -> st {login = l})
                pure a
              eval (SetAuth a) = do
                st <- get
                let t = (AuthToken st.login)
                put $ st {auth = Just t}
                sub <- liftEff $ initSubscriber t 
                subscribe (chatMessages sub.messages)
                modify (\s -> s {sub = true})
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

systemMessage :: String -> ChatMessage
systemMessage s = ChatMessage {userName: "System", messageBody: s}

chatMessages ::  forall eff.  Signal Action ->  EventSource Query (Aff(Effects eff))
chatMessages sig = eventSource (callback sig) (\a -> case a of
            Update s -> f $ UpdateText s
            Nop -> f $ UpdateText [systemMessage "No op"]
            ReportError -> f $ UpdateText [systemMessage "Error"]
            SubscriberLog s -> f $ UpdateText [systemMessage s]
        )
        where 
            f :: (SubscribeStatus -> Query SubscribeStatus) -> Maybe (Query SubscribeStatus)
            f x = Just $ x Listening


sendMessage :: forall eff. String -> AuthToken -> Aff (ajax :: AJAX | eff) (Maybe String)
sendMessage s a = do
    ebs <- runExceptT $ runReaderT (postChat a s) settings
    pure $ case ebs of
        Left err -> Just $ errorToString err
        _ -> Nothing
