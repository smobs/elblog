module Component.About where

import Prelude
import Halogen

import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import Data.Void
import Data.Maybe (Maybe(..))
foreign import mePic :: String

type State = Unit

data Query a = Noop a

type Input = Unit

type Message = Void

about :: forall g . Component H.HTML Query Input Message g 
about = component 
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing}
            where
              render :: State -> ComponentHTML Query
              render _ = H.div_
                         [ H.h2_ [H.text "Me"]
                         , H.img [P.src mePic, P.height 200]
                         ,  H.h2_ [H.text "Source"]
                         , H.div_
                           [ H.a
                             [ P.href "https://github.com/smobs/elblog"
                             , P.target "_blank"
                             ]
                             [H.text "On Github"]
                           ]
                         ]

              eval :: Query ~> (ComponentDSL State Query Message g)
              eval (Noop a) = pure a
