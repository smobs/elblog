{-# LANGUAGE DeriveGeneric
           , DataKinds
           , KindSignatures
           , InstanceSigs
           , RecordWildCards #-}
module Data.Wizard where

import GHC.Generics
import qualified Data.Wizard.Command as Com
import Data.Text (Text)
import GHC.TypeLits
import Data.FiniteDouble
import Control.Arrow((***))
import Data.ComponentSystem
import Data.Functor.Apply
type Vector = (Double, Double)
type GamePosition = (FiniteDouble 1000, FiniteDouble 500)

type Velocity = Vector
data GameState = GameState { positionSys :: ComponentSystem GamePosition
                           , velocitySys :: ComponentSystem Vector} 

type PlayerId = EntityId

initialState :: GameState
initialState = GameState newSystem newSystem

getDimensions :: (Int, Int)
getDimensions = let (x, y) = (finiteZero, finiteZero) :: GamePosition 
                in (fromIntegral $ natVal x, fromIntegral $ natVal y)

addPlayer :: PlayerId -> GameState -> GameState
addPlayer pid g@(GameState{..})= g {positionSys = addComponent pid (finiteZero,finiteZero) positionSys, velocitySys = addComponent pid (0, 0) velocitySys}


removePlayer :: PlayerId -> GameState -> GameState
removePlayer n (GameState poss velocitys) = GameState (deleteComponent n poss) (deleteComponent n velocitys) 

updateGame :: PlayerId -> Com.GameCommand -> GameState -> GameState
updateGame pId (Com.Move d) g@(GameState{..}) = g {velocitySys = updateComponent (Just . setVelocity 20 d) pId velocitySys}
updateGame pId (Com.StopMove d) g@(GameState{..}) = g {velocitySys = updateComponent (Just . stopVelocity d) pId velocitySys}

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