{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, RecordWildCards #-}
module Data.Wizard.View where 

import GHC.Generics
import Data.Wizard
import Data.FiniteDouble
import Data.ComponentSystem
import Data.Functor.Apply((<.>))
import Data.Wizard.Model

data GameView = GameView {terrain :: [Terrain], players :: [PlayerView], dimensions :: (Int, Int)} deriving (Generic, Eq, Show)

data Colour = Colour Int Int Int deriving (Generic, Eq, Show)
data PlayerView = PlayerView Position Shape Colour PlayerId deriving (Generic, Eq, Show)

data Shape = Rectangle Double Double deriving (Generic, Eq, Show)

data Position = Position Double Double deriving (Generic, Eq, Show)

data Terrain = Terrain Position Shape deriving (Generic, Eq, Show)

stateToGameView :: GameState -> GameView
stateToGameView g = GameView (getTerrain g) (getPlayers g) (getDimensions)

getPlayers :: GameState -> [PlayerView]
getPlayers (GameState {..}) = snd <$> (listComponents $ toPlayerView <$>  playerSys <.> positionSys <.> boundSys)

getTerrain :: GameState -> [Terrain]
getTerrain GameState{..} = snd <$> (listComponents $ mkTerrain <$> (asMarker terrainSys positionSys) <.> boundSys)
    where mkTerrain (x, y) (s) = Terrain (Position (getFinite x) (getFinite y)) (boundToShape s)

toPlayerView :: PlayerId -> GamePosition -> Bounds -> PlayerView
toPlayerView n (x,y) b = 
        PlayerView (Position (getFinite x) (getFinite y)) (boundToShape b) (Colour 256 0 0) n

boundToShape :: Bounds -> Shape
boundToShape (RectangleBound w h) = Rectangle (getFinite w) (getFinite h)