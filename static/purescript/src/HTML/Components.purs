module HTML.Components where

import Prelude
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Halogen.HTML.CSS.PureCSS as Pure

import Data.Tuple



nav :: forall p i . String -> Array (Tuple String String) ->  HTML p i
nav title hs = H.div [P.classes Pure.horizontalMenu]
                          [ H.a [P.href "/#/", P.classes [Pure.menuHeading, Pure.menuLink]] [H.text title]
                          , H.ul [P.class_ Pure.menuList]
                            (map (\(Tuple n l) -> H.li [P.class_ Pure.menuItem]
                                                  [H.a [P.class_ Pure.menuLink , P.href l]
                                                    [H.text n]])
                             hs)]
