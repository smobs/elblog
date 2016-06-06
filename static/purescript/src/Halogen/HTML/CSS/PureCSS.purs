module Halogen.HTML.CSS.PureCSS where

import Prelude
import Halogen.HTML.Core (className, ClassName)

grid :: ClassName
grid = className "pure-g"

u :: Int -> Int -> ClassName
u i n = className $ "pure-u-" ++ (show i) ++ "-" ++ (show n)

button :: ClassName
button = className "pure-button"

buttonPrimary :: ClassName
buttonPrimary = className "pure-button-primary"
