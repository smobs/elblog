{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, RecordWildCards #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard
import Data.FiniteDouble
import Data.ComponentSystem

data GameView = GameView {terrain :: [Terrain], players :: [PlayerView], dimensions :: (Int, Int)} deriving (Generic, Eq, Show)

data Colour = Colour Int Int Int deriving (Generic, Eq, Show)
data PlayerView = PlayerView Position Shape Colour PlayerId deriving (Generic, Eq, Show)

data Shape = Rectangle Double Double deriving (Generic, Eq, Show)

data Position = Position Double Double deriving (Generic, Eq, Show)

data Terrain = Terrain Position Shape deriving (Generic, Eq, Show)

stateToGameView :: GameState -> GameView
stateToGameView g = GameView [] (toPlayerView <$> getPlayers g) (getDimensions)

getPlayers :: GameState -> [(PlayerId, GamePosition)]
getPlayers (GameState {..}) = listComponents $ asMarker playerSys positionSys

toPlayerView :: (PlayerId, GamePosition) -> PlayerView
toPlayerView (n,(x,y)) = 
        PlayerView (Position (getFinite x) (getFinite y))(Rectangle 10 10) (Colour 256 0 0) n