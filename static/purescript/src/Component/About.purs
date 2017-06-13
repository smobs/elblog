module Component.About where

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties (pixels)

foreign import mePic :: String

type State = Unit

data Query a = Noop a

about :: forall g . Component State Query g 
about = component {render, eval}
            where
              render :: State -> ComponentHTML Query
              render _ = H.div_
                         [ H.h2_ [H.text "Me"]
                         , H.img [P.src mePic, P.height $ pixels 200]
                         ,  H.h2_ [H.text "Source"]
                         , H.div_
                           [ H.a
                             [ P.href "https://github.com/smobs/elblog"
                             , P.target "_blank"
                             ]
                             [H.text "On Github"]
                           ]
                         ]

              eval :: Query ~> (ComponentDSL State Query g)
              eval (Noop a) = pure a
