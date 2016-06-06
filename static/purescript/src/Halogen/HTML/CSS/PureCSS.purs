module Halogen.HTML.CSS.PureCSS where

import Prelude

import Halogen.HTML.Core (className, ClassName, HTML)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import Data.Tuple

grid :: ClassName
grid = className "pure-g"

u :: Int -> Int -> ClassName
u i n = className $ "pure-u-" ++ (show i) ++ "-" ++ (show n)

button :: ClassName
button = className "pure-button"

buttonPrimary :: ClassName
buttonPrimary = className "pure-button-primary"


horizontalMenu :: forall p i . String -> Array (Tuple String String) -> HTML p i
horizontalMenu title hs = H.div [P.classes $ map className ["pure-menu", "pure-menu-horizontal"]]
                          [ H.a [P.href "/#/", P.classes $ map className ["pure-menu-heading", "pure-menu-link"]] [H.text title]
                          , H.ul [P.class_ $ className "pure-menu-list"]
                            (map (\(Tuple n l) -> H.li [P.class_ $ className "pure-menu-item"]
                                                  [H.a [P.class_ $ className "pure-menu-link" , P.href l]
                                                    [H.text n]])
                             hs)]
