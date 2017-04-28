{-# LANGUAGE DataKinds #-}
module Data.Wizard.Model where


import Data.FiniteDouble
import Data.ComponentSystem
import Data.Text (Text)
import Data.Set (Set, empty) 
type Vector = (Double, Double)
type GameWidth = FiniteDouble 1000
type GameHeight = FiniteDouble 500
type GamePosition = (GameWidth, GameHeight)

type Velocity = Vector

type PlayerId = Text

data Bounds = RectangleBound GameWidth GameHeight deriving Show

data TerrainState = Points (Set (Int, Int)) Bool

data GameState = GameState { positionSys :: ComponentSystem GamePosition
                           , velocitySys :: ComponentSystem Vector
                           , playerSys :: ComponentSystem PlayerId
                           , boundSys :: ComponentSystem Bounds
                           , terrainSys :: ComponentSystem ()
                           , terrainState :: TerrainState} 

emptyGameState :: GameState
emptyGameState = GameState { positionSys = newSystem
                                      , velocitySys = newSystem
                                      , playerSys = newSystem 
                                      , boundSys = newSystem 
                                      , terrainSys = newSystem
                                      , terrainState = Points empty False}