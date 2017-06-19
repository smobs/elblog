module Component.LoginWidget where
  
import Chat.ServerTypes
import Prelude
import Data.Maybe (Maybe(..))  

import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import DOM.Event.KeyboardEvent as K

type State = {auth :: Maybe AuthToken, login :: String}

onSpecificKeyPress :: forall r i. String -> (K.KeyboardEvent -> Maybe i) -> P.IProp (onKeyPress :: K.KeyboardEvent | r) i
onSpecificKeyPress code = E.onKeyPress    

render update setAuth = H.div_ $ [ H.text "Username", H.input [E.onValueInput (E.input update), onSpecificKeyPress "Enter" (E.input_ setAuth)], H.button [E.onClick (E.input_ setAuth)] [H.text "Login"] ]