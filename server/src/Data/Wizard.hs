{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard where

import GHC.Generics
import qualified Data.Wizard.Command as Com

data GameState = GameState (Int,Int) deriving (Eq, Show)


initialState :: GameState
initialState = GameState (0,0)

updateGame :: Com.GameCommand -> GameState -> GameState
updateGame (Com.Move d) (GameState (x,y)) = case d of
    Com.Up -> GameState (x, y - 5)
    Com.Down -> GameState (x, y + 5)
    Com.Left -> GameState (x - 5, y)
    Com.Right -> GameState (x + 5, y)
updateGame _ g = g