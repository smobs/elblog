{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard

data GameView = GameView {terrain :: [Shape]} deriving (Generic, Eq, Show)

data Shape = Rectangle Double Double Double Double deriving (Generic, Eq, Show)

stateToGameView :: GameState -> GameView
stateToGameView (GameState i) = let x = fromIntegral i in GameView [Rectangle x x x x]