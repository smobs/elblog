module Halogen.HTML.CSS.PureCSS where

import Prelude

import Halogen.HTML.Core (ClassName(..), HTML, Prop)

grid :: ClassName
grid = ClassName "pure-g"

u :: Int -> Int -> ClassName
u i n = ClassName $ "pure-u-" <> (show i) <> "-" <> (show n)

button :: ClassName
button = ClassName "pure-button"

buttonPrimary :: ClassName
buttonPrimary = ClassName "pure-button-primary"

horizontalMenu :: Array ClassName
horizontalMenu = map ClassName ["pure-menu", "pure-menu-horizontal"]

menuLink :: ClassName
menuLink = ClassName "pure-menu-link"

menuHeading :: ClassName
menuHeading = ClassName "pure-menu-heading"

menuItem :: ClassName
menuItem = ClassName "pure-menu-item"

menuList :: ClassName
menuList = ClassName "pure-menu-list"
