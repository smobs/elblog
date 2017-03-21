{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard where

import GHC.Generics
import qualified Data.Wizard.Command as Com

data GameState = GameState Int deriving (Eq, Show)


updateGame :: Com.GameCommand -> GameState -> GameState
updateGame (Com.Move d) _ = case d of
    Com.Up -> GameState 0
    Com.Down -> GameState 50
    Com.Left -> GameState 100
    Com.Right -> GameState 150