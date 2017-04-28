{-# LANGUAGE RecordWildCards #-}

module Data.ComponentSystem.Terrain (
    updateTerrain,
    newTerrainState
    ) where
import Prelude hiding (Right)
import Data.ComponentSystem
import Data.Wizard.Model
import Data.FiniteDouble
import qualified Data.Set as S

type Right = Int
type Top = Int
type Width = Int
type Height = Int

type Square = ((Right, Top), (Width, Height))

pointsToSquares :: (Int, Int) -> S.Set (Int, Int) -> [Square]
pointsToSquares _ s = maybe [] f $ do
        (mi, _)  <- S.minView s
        (ma, _)  <- S.maxView s
        pure (mi, ma)
    where f ((x,y), (a,b)) = [ ((x,y), (a - x, b - y))]



addTerrainBlock :: Square -> GameState -> IO GameState
addTerrainBlock ((r,t), (w, h)) g@GameState{..} = do 
                                    i <- createId
                                    let add = addComponent i
                                    pure $ g { terrainSys = add () terrainSys
                                             , positionSys = add (clampedCast (fromIntegral r),clampedCast (fromIntegral t)) positionSys 
                                             , boundSys =  add (RectangleBound (clampedCast (fromIntegral w)) (clampedCast (fromIntegral h))) boundSys} 
updateTerrain :: GameState -> IO GameState
updateTerrain g@GameState{..} = let Points ts dirty = terrainState 
                                in if dirty
                                   then 
                                       let pSys = clearSys (asMarker terrainSys) positionSys
                                           bSys = clearSys (asMarker terrainSys) boundSys
                                           g' = g { positionSys = pSys
                                                  , boundSys = bSys
                                                  , terrainSys = newSystem
                                                  , terrainState = Points ts False }
                                           sqs = pointsToSquares (1000, 500) ts
                                       in foldr (\s m -> m >>= addTerrainBlock s) (pure g') sqs   
                                       
                                   else pure g 


newTerrainState :: TerrainState
newTerrainState = Points (S.fromList [(x,y) | x <- [0 .. 1000], y <- [0 .. x]]) True
