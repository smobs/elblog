{-# LANGUAGE RecordWildCards #-}

module Data.ComponentSystem.Terrain (
    updateTerrain,
    newTerrainState
    ) where
import Prelude hiding (Left)
import Data.ComponentSystem
import Data.Wizard.Model
import Data.FiniteDouble
import qualified Data.Set as S

type Left = Int
type Top = Int
type Width = Int
type Height = Int

type Square = ((Left, Top), (Width, Height))

pointsToSquares :: S.Set (Int, Int) -> [Square]
pointsToSquares s = case expand s of
                Nothing -> []
                Just (sq, s') -> sq : pointsToSquares s'
    
expand :: S.Set (Int, Int) -> Maybe (Square, S.Set (Int, Int))
expand s = uncurry (expand') <$> (S.minView s)

expand' :: (Int, Int) -> S.Set (Int,Int) -> (Square, S.Set (Int, Int))
expand' cur s = expandSquare ((cur, (10,10))) s

expandSquare :: Square -> S.Set (Int, Int) -> (Square, S.Set(Int, Int))
expandSquare s = (uncurry expandRight) . expandDown s
                                    

expandRight :: Square -> S.Set (Int, Int) -> (Square, S.Set(Int, Int))
expandRight ((x,y) ,(w,h)) s = 
        let next = (x + w, y)
        in if S.member next s
                 then expandRight  ((x,y), (w + 10, h))  (S.delete next s)
                 else (((x,y),(w,h)), s)

expandDown :: Square -> S.Set (Int, Int) -> (Square, S.Set(Int, Int))
expandDown ((x,y) ,(w,h)) s = 
        let area = S.fromList [(x', y + h) | x' <- [x .. (x+w)]]
        in if S.isSubsetOf area s
                 then expandDown ((x,y), (w , h + 10))  (S.difference s area)
                 else (((x,y),(w,h)), s)


addTerrainBlock :: Square -> GameState -> IO GameState
addTerrainBlock ((l,t), (w, h)) g@GameState{..} = do 
                                    i <- createId
                                    let add = addComponent i
                                    pure $ g { terrainSys = add () terrainSys
                                             , positionSys = add (clampedCast (f l w),clampedCast (f t h)) positionSys 
                                             , boundSys =  add (RectangleBound (clampedCast (fromIntegral w)) (clampedCast (fromIntegral h))) boundSys} 
                    where f p s = (fromIntegral p) + (fromIntegral s / 2)

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
                                           sqs = pointsToSquares ts
                                       in do 
                                           putStrLn "doing dirty work"
                                           foldr (\s m -> m >>= addTerrainBlock s) (pure g') sqs   
                                       
                                   else pure g 


newTerrainState :: TerrainState
newTerrainState = Points (S.fromList [(x,y) | x <- [0 .. 1000], y <- [0 .. 500 - x], mod x 10 == 0, mod y 10 == 0]) True
