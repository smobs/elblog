module Graphics.Game where

import Prelude
import Color
import Data.Int
import Data.Array
import Data.Monoid
import Data.Foldable
import Data.Tuple
import Chat.ServerTypes
import Data.Wizard.View as View
import Control.Category ((<<<))
import Control.Monad.Eff (Eff)
import Data.Function ((<<<))
import Data.Maybe (Maybe(..))
import Data.Wizard.View (GameView(..))
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, CANVAS)
import Graphics.Drawing (Drawing, Shape, black, fillColor, filled, rectangle, render, text, scale)
import Graphics.Drawing.Font

renderGame :: forall eff. Number -> Number -> String -> GameView -> Eff (canvas :: CANVAS | eff) Unit
renderGame cw ch id g =  do
  mCanvas <- getCanvasElementById id
  case mCanvas of
    Just canvas -> do 
      ctx <- getContext2D canvas
      clearRect ctx {x: 0.0, y: 0.0, w: cw, h: ch} 
      render ctx $ drawState cw ch g
    Nothing -> pure unit


drawState :: Number -> Number -> GameView -> Drawing
drawState cw ch (GameView {terrain, players, dimensions}) = scaleGame dimensions cw ch $ drawTerrain terrain <> (fold $ drawPlayer <$> players)

scaleGame :: Tuple Int Int -> Number -> Number -> Drawing -> Drawing
scaleGame (Tuple gw gh) cw ch = scale (cw / (toNumber gw)) (ch / (toNumber gh))

drawTerrain :: Array (View.Terrain) -> Drawing
drawTerrain = filled (fillColor black) <<< fold <<< (<$>) (\(View.Terrain pos shape) ->  drawShape pos shape) 

drawPlayer :: View.PlayerView -> Drawing
drawPlayer (View.PlayerView (p@(View.Position x y)) s (View.Colour r g b) n) =  (filled (fillColor (rgb r g b)) $ drawShape p s) <> (text (font serif 12 mempty) x y (fillColor black) n)

drawShape :: View.Position -> View.Shape -> Shape
drawShape (View.Position x y) (View.Rectangle w h) = rectangle (x - w /2.0) (y - h/2.0) w h

