module Component.About where

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

type State = Unit

data Query a = Noop a

about :: forall g . Component State Query g 
about = component {render, eval}
            where
              render :: State -> ComponentHTML Query
              render _ = H.div_
                         [ H.h2_ [H.text "Source"]
                         , H.div_
                           [ H.a
                             [ P.href "https://github.com/smobs/elblog"
                             , P.target "_blank"
                             ]
                             [H.text "On Github"]
                           ]
                         ]

              eval :: Natural Query (ComponentDSL State Query g)
              eval (Noop a) = return a
