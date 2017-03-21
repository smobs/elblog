{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard

data GameView = GameView Int deriving (Generic, Eq, Show)


stateToGameView :: GameState -> GameView
stateToGameView (GameState i) = GameView i