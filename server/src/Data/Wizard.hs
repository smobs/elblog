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

type Speed = Vector
data GameState = GameState { positionSys :: ComponentSystem GamePosition
                           , speedSys :: ComponentSystem Vector} 

type PlayerId = EntityId

initialState :: GameState
initialState = GameState newSystem newSystem

getDimensions :: (Int, Int)
getDimensions = let (x, y) = (finiteZero, finiteZero) :: GamePosition 
                in (fromIntegral $ natVal x, fromIntegral $ natVal y)

addPlayer :: PlayerId -> GameState -> GameState
addPlayer pid g@(GameState{..})= g {positionSys = addComponent pid (finiteZero,finiteZero) positionSys, speedSys = addComponent pid (0, 0) speedSys}


removePlayer :: PlayerId -> GameState -> GameState
removePlayer n (GameState poss speeds) = GameState (deleteComponent n poss) (deleteComponent n speeds) 

updateGame :: PlayerId -> Com.GameCommand -> GameState -> GameState
updateGame pId (Com.Move d) g@(GameState{..}) = g {speedSys = updateComponent (Just . adjustSpeed d) pId speedSys}
updateGame pId (Com.Configuration Com.AddPlayer) g =  addPlayer pId g 
updateGame pId (Com.Configuration Com.RemovePlayer) g =  removePlayer pId g

stepGame :: Double -> GameState -> IO GameState 
stepGame i g@GameState{..} = do
    let s' = liftF2 (move i) speedSys positionSys
    pure g {positionSys = s'}

adjustSpeed :: Com.Direction -> Speed -> Speed
adjustSpeed d (x,y) = case d of
                                Com.Up -> (x, -5)
                                Com.Down -> (x, 5)
                                Com.Left -> (-5, y)
                                Com.Right -> (5, y)

move :: (KnownNat w, KnownNat h) => Double -> Vector -> (FiniteDouble w, FiniteDouble h) -> (FiniteDouble w, FiniteDouble h)
move delta (vecx, vecy) p =  (\ (fx, fy) -> (fx$ vecx*delta, fy $ vecy * delta )) $ (clampedAdd *** clampedAdd) p
 