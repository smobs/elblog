{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard where

import GHC.Generics
import qualified Data.Wizard.Command as Com
import Data.Map.Strict as M
import Data.Text (Text)

data GameState = GameState (M.Map PlayerId (Int,Int)) deriving (Eq, Show)

type PlayerId = Text 

initialState :: GameState
initialState = GameState M.empty


addPlayer :: PlayerId -> GameState -> GameState
addPlayer n (GameState ps) = GameState $ M.insert n (0,0) ps

updateGame :: PlayerId -> Com.GameCommand -> GameState -> GameState
updateGame pId (Com.Move d) (GameState ps) = GameState $ M.update (Just . move d) pId ps
updateGame pId (Com.Configuration Com.AddPlayer) g =  addPlayer pId g
updateGame _ _ g = g


move :: (Com.Direction) -> (Int, Int) -> (Int, Int)
move d (x,y) = case d of
    Com.Up -> (x, y - 5)
    Com.Down -> (x, y + 5)
    Com.Left -> (x - 5, y)
    Com.Right -> (x + 5, y)