module Graphics.Game where

import Prelude
import Data.Int
import Data.Array
import Control.Category ((<<<))
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Monoid
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, CANVAS)
import Graphics.Drawing (fillColor, Drawing, render, rectangle, filled, black)
import Data.Foldable
import Chat.ServerTypes

renderGame :: forall eff. Number -> Number -> String -> GameState -> Eff (canvas :: CANVAS | eff) Unit
renderGame cw ch id g =  do
  mCanvas <- getCanvasElementById id
  case mCanvas of
    Just canvas -> do 
      ctx <- getContext2D canvas
      clearRect ctx {x: 0.0, y: 0.0, w: cw, h: ch} 
      render ctx $ drawState g
    Nothing -> pure unit



drawState :: GameState -> Drawing
drawState (GameState i) =  filled (fillColor black) $ fold $ (\x -> (  rectangle x x 1.0 1.0)) <$> toNumber <$> range 0 i
    