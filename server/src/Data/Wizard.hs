{-# LANGUAGE DeriveGeneric
           , DataKinds
           , KindSignatures
           , InstanceSigs
           , RecordWildCards #-}
module Data.Wizard where

import qualified Data.Wizard.Command as Com
import qualified Data.Text as T
import GHC.TypeLits
import Data.FiniteDouble
import Control.Arrow((***))
import Data.ComponentSystem
import Data.Functor.Apply
type Vector = (Double, Double)
type GameWidth = FiniteDouble 1000
type GameHeight = FiniteDouble 500
type GamePosition = (GameWidth, GameHeight)


data Bounds = RectangleBound GameWidth GameHeight 
              | CompositeBound Bounds Bounds

type Velocity = Vector
data GameState = GameState { positionSys :: ComponentSystem GamePosition
                           , velocitySys :: ComponentSystem Vector
                           , playerSys :: ComponentSystem PlayerId
                           , boundSys :: ComponentSystem Bounds
                           , terrainSys :: ComponentSystem ()} 

type PlayerId = T.Text

initialState :: GameState
initialState = addTerrain $ GameState newSystem newSystem newSystem newSystem newSystem

addTerrain :: GameState -> GameState
addTerrain g = foldr id g [addTerrainBlock (clampedAdd finiteZero x, clampedAdd finiteZero y) | x <- [50 .. 100], y <- [100 .. 200]]

getDimensions :: (Int, Int)
getDimensions = let (x, y) = (finiteZero, finiteZero) :: GamePosition 
                in (fromIntegral $ natVal x, fromIntegral $ natVal y)

addPlayer :: PlayerId -> GameState -> GameState
addPlayer pid g@(GameState{..})= let add = addComponent (TextId pid) 
                                     size = fromIntegral $ 5 * T.length pid
                                 in
    g { positionSys = add (finiteZero,finiteZero) positionSys
      , velocitySys = add (0, 0) velocitySys
      , playerSys = add pid playerSys
      , boundSys = add (RectangleBound (clampedAdd finiteZero size) (clampedAdd finiteZero size)) boundSys}



addTerrainBlock :: GamePosition -> GameState -> GameState
addTerrainBlock pos g@GameState{..} = let add = addComponent  (TextId $ T.pack "TerrainId")
                                    in g { terrainSys = add () terrainSys
                                         , positionSys = add pos positionSys 
                                         , boundSys =  add (RectangleBound (clampedAdd finiteZero 1) (clampedAdd finiteZero 1)) boundSys} 



removePlayer :: PlayerId -> GameState -> GameState
removePlayer n g@(GameState {..}) = let del = deleteComponent (TextId n) 
                                                      in g { positionSys = del positionSys
                                                           , velocitySys = del velocitySys 
                                                           , playerSys = del playerSys}

updateGame :: PlayerId -> Com.GameCommand -> GameState -> GameState
updateGame pId (Com.Move d) g@(GameState{..}) = g {velocitySys = updateComponent (Just . setVelocity 20 d) (TextId pId) velocitySys}
updateGame pId (Com.StopMove d) g@(GameState{..}) = g {velocitySys = updateComponent (Just . stopVelocity d) (TextId pId) velocitySys}
updateGame pId (Com.Configuration Com.AddPlayer) g =  addPlayer pId g 
updateGame pId (Com.Configuration Com.RemovePlayer) g =  removePlayer pId g

stepGame :: Double -> GameState -> IO GameState 
stepGame i g@GameState{..} = do
    let s' = liftF2 (move i) velocitySys positionSys
    pure g {positionSys = s'}

setVelocity :: Double -> Com.Direction -> Velocity -> Velocity
setVelocity speed d (x,y) = case d of
                                Com.Up -> (x, -speed)
                                Com.Down -> (x, speed)
                                Com.Left -> (-speed, y)
                                Com.Right -> (speed, y)

stopVelocity :: Com.Direction -> Velocity -> Velocity
stopVelocity d (x,y) = case d of
                                Com.Up -> (x, 0)
                                Com.Down -> (x, 0)
                                Com.Left -> (0, y)
                                Com.Right -> (0, y)

move :: (KnownNat w, KnownNat h) => Double -> Vector -> (FiniteDouble w, FiniteDouble h) -> (FiniteDouble w, FiniteDouble h)
move delta (vecx, vecy) p =  (\ (fx, fy) -> (fx$ vecx*delta, fy $ vecy * delta )) $ (clampedAdd *** clampedAdd) p