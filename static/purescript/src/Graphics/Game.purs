module Graphics.Game where

import Prelude
import Color
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
import Graphics.Drawing (Drawing, Shape, black, fillColor, filled, rectangle, render, text)
import Graphics.Drawing.Font

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
drawState cw (GameView {terrain, players}) =  drawTerrain terrain <> (fold $ drawPlayer <$> players)

drawTerrain :: Array (View.Terrain) -> Drawing
drawTerrain = filled (fillColor black) <<< fold <<< (<$>) (\(View.Terrain pos shape) ->  drawShape pos shape) 

drawPlayer :: View.PlayerView -> Drawing
drawPlayer (View.PlayerView (p@(View.Position x y)) s (View.Colour r g b) n) = (text (font serif 12 mempty) x y (fillColor black) n) <> (filled (fillColor (rgb r g b)) $ drawShape p s)

drawShape :: View.Position -> View.Shape -> Shape
drawShape (View.Position x y) (View.Rectangle w h) = rectangle x y w h

