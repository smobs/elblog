module Component.LoginWidget where
  
import Chat.ServerTypes
import Prelude
import Data.Maybe (Maybe(..))  

import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import DOM.Event.KeyboardEvent as K

type State = {auth :: Maybe AuthToken, login :: String}


onlyForKey :: forall props g ev.                       
  (Functor g) => String                      
                   -> (K.KeyboardEvent                      
                       -> g (Maybe ev)     
                      )                       
                      -> K.KeyboardEvent
                         -> g (Maybe ev)
onlyForKey i input  = (\ (ke) -> (if K.code ke == i then id else const Nothing) <$> input ke)

render update setAuth = H.div_ $ [ H.text "Username",  H.input [E.onValueInput (E.input update), E.onKeyPress  (onlyForKey "Enter" (E.input_ setAuth))], H.button [E.onClick (E.input_ setAuth)] [H.text "Login"] ]