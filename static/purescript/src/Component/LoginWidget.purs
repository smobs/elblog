module Component.LoginWidget where
  
import Chat.ServerTypes
import Prelude
import Data.Maybe (Maybe(..))  

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

type State = {auth :: Maybe AuthToken, login :: String}


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

render update setAuth = H.div_ $ [ H.text "Username",  H.input [E.onValueInput (E.input update), E.onKeyPress  (onlyForKey 13.0 (E.input_ setAuth))], H.button [E.onClick (E.input_ setAuth)] [H.text "Login"] ]
              