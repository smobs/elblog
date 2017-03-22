{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard
import qualified Data.Map as M
data GameView = GameView {terrain :: [Shape], players :: [PlayerView]} deriving (Generic, Eq, Show)
data Colour = Colour Int Int Int deriving (Generic, Eq, Show)
data PlayerView = PlayerView Shape Colour PlayerId deriving (Generic, Eq, Show)
data Shape = Rectangle Double Double Double Double deriving (Generic, Eq, Show)

stateToGameView :: GameState -> GameView
stateToGameView (GameState ps) = GameView [] (toPlayerView <$> M.toList ps)

toPlayerView :: (PlayerId, (Int, Int)) -> PlayerView
toPlayerView (n, (x, y)) = PlayerView (Rectangle (fromIntegral x) (fromIntegral y) 10 10) (Colour 256 0 0) n