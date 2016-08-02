module Halogen.HTML.CSS.PureCSS where

import Prelude

import Halogen.HTML.Core (className, ClassName, HTML, Prop)

grid :: ClassName
grid = className "pure-g"

u :: Int -> Int -> ClassName
u i n = className $ "pure-u-" <> (show i) <> "-" <> (show n)

button :: ClassName
button = className "pure-button"

buttonPrimary :: ClassName
buttonPrimary = className "pure-button-primary"

horizontalMenu :: Array ClassName
horizontalMenu = map className ["pure-menu", "pure-menu-horizontal"]

menuLink :: ClassName
menuLink = className "pure-menu-link"

menuHeading :: ClassName
menuHeading = className "pure-menu-heading"

menuItem :: ClassName
menuItem = className "pure-menu-item"

menuList :: ClassName
menuList = className "pure-menu-list"
