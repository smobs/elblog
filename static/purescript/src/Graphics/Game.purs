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
import Data.Wizard.View

renderGame :: forall eff. Number -> Number -> String -> GameView -> Eff (canvas :: CANVAS | eff) Unit
renderGame cw ch id g =  do
  mCanvas <- getCanvasElementById id
  case mCanvas of
    Just canvas -> do 
      ctx <- getContext2D canvas
      clearRect ctx {x: 0.0, y: 0.0, w: cw, h: ch} 
      render ctx $ drawState cw g
    Nothing -> pure unit



drawState :: Number -> GameView -> Drawing
drawState cw (GameView i) =  filled (fillColor black) $ fold $ (\x -> rectangle (toNumber $ mod (floor x) (floor cw)) (5.0 * (toNumber $ div (floor x) (floor cw)))  5.0 5.0) <$> (\x -> 5.0 * toNumber x)  <$> range 0 (i * 50)
    