{-# LANGUAGE DeriveGeneric
           , DataKinds
           , KindSignatures
           , InstanceSigs #-}
module Data.Wizard where

import GHC.Generics
import qualified Data.Wizard.Command as Com
import Data.Map.Strict as M
import Data.Text (Text)
import GHC.TypeLits
import Data.FiniteDouble


type PlayerPosition = (FiniteDouble 1000, FiniteDouble 500)

data GameState = GameState (M.Map PlayerId PlayerPosition) deriving (Eq, Show)

type PlayerId = Text

initialState :: GameState
initialState = GameState M.empty


getDimensions :: (Int, Int)
getDimensions = let (x, y) = (finiteZero, finiteZero) :: PlayerPosition 
                in (fromIntegral $ natVal x, fromIntegral $ natVal y)

addPlayer :: PlayerId -> GameState -> GameState
addPlayer n (GameState ps) = GameState $ M.insert n (finiteZero,finiteZero) ps

removePlayer :: PlayerId -> GameState -> GameState
removePlayer n (GameState ps) = GameState $ M.delete n ps

updateGame :: PlayerId -> Com.GameCommand -> GameState -> GameState
updateGame pId (Com.Move d) (GameState ps) = GameState $ M.update (Just . move d) pId ps
updateGame pId (Com.Configuration Com.AddPlayer) g =  addPlayer pId g
updateGame pId (Com.Configuration Com.RemovePlayer) g =  removePlayer pId g

stepGame :: Double -> GameState -> IO GameState 
stepGame i g = do
    putStrLn (show i) 
    pure g

move :: (KnownNat w, KnownNat h) => (Com.Direction) -> (FiniteDouble w, FiniteDouble h) -> (FiniteDouble w, FiniteDouble h)
move d (x,y) = let inc = toBoundedDouble 5 
               in case d of
                Com.Up -> (x, clampedSubtract y inc)
                Com.Down -> (x, clampedAdd y inc)
                Com.Left -> (clampedSubtract x inc, y)
                Com.Right -> (clampedAdd x inc, y)

