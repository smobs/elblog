{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard

data GameView = GameView {terrain :: [Shape]} deriving (Generic, Eq, Show)

data Shape = Rectangle Double Double Double Double deriving (Generic, Eq, Show)

stateToGameView :: GameState -> GameView
stateToGameView (GameState (x,y)) = GameView [Rectangle (fromIntegral x) (fromIntegral y) 10 10]