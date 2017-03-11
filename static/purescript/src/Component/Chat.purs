module Component.Chat where

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Control.Monad (class Monad, pure, bind)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Var (class Updatable)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Halogen (Component, ComponentDSL, ComponentHTML, HalogenEffects, action)
import Halogen.Component (lifecycleComponent)
import Halogen.Query (set)
import Network.HTTP.Affjax (AJAX, get)
import Signal (Signal)
import Signal.Channel (Channel, subscribe, send, channel, CHANNEL)
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
              eval (Connect a) = pure a
              eval (Disconnect a) = pure a
              eval (UpdateText t a) = do
                set {text: t}
                pure a

