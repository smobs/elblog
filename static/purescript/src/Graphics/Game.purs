module Graphics.Game where

import Prelude
import Data.Int
import Data.Array
import Data.Monoid
import Data.Foldable
import Chat.ServerTypes
import Data.Wizard.View as View
import Control.Category ((<<<))
import Control.Monad.Eff (Eff)
import Data.Function ((<<<))
import Data.Maybe (Maybe(..))
import Data.Wizard.View (GameView(..))
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, CANVAS)
import Graphics.Drawing (Drawing, Shape, black, fillColor, filled, rectangle, render)

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
drawState cw (GameView {terrain}) =  drawTerrain terrain

drawTerrain :: Array (View.Shape) -> Drawing
drawTerrain = filled (fillColor black) <<< fold <<< (<$>) drawShape 

drawShape :: View.Shape -> Shape
drawShape (View.Rectangle x y w h) = rectangle x y w h

