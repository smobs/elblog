module Pong where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, CANVAS)
import Graphics.Drawing (render, rectangle, filled, black, fillColor)

type BatState = {pos :: Number, dir :: Move}

newtype PongState = PongState { p1 :: BatState
                              , p2 :: BatState
                              , bx :: Number
                              , by :: Number
                              , bdirX :: Number
                              , bdirY :: Number}


data Player = One | Two
data Move = Up | Down | Stop
data PongCommand = MovePlayer {player :: Player, move :: Move} | Step

initial :: PongState
initial = PongState {p1: {pos:0.0, dir: Stop}, p2: {pos:0.0, dir: Stop}, bx: 40.0, by: 40.0, bdirX: 1.0, bdirY: 1.0}

sendCommand :: PongCommand -> PongState -> PongState
sendCommand (MovePlayer {player, move}) (PongState s) = PongState (
   case player of
     One -> s {p1 = s.p1 {dir = move}}
     Two -> s {p2 = s.p2 {dir = move}})
sendCommand Step (PongState s) = PongState (
  s { p1 = updateBat s.p1
    , p2 = updateBat s.p2})

updateBat :: BatState -> BatState
updateBat bs@{pos, dir} =
  case dir of
    Stop -> bs
    Up -> bs {pos = pos - 1.0}
    Down -> bs {pos = pos + 1.0}
    
renderPong :: String -> PongState -> Eff (canvas :: CANVAS) Unit
renderPong id (PongState s) =  do
  mCanvas <- getCanvasElementById id
  case mCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      clearRect ctx {x: 0.0, y: 0.0, w: canvasSize.w, h: canvasSize.h} 
      render ctx $
        filled (fillColor black) (rectangle 0.0 s.p1.pos batSize.w batSize.h)
      render ctx $
        filled (fillColor black) (rectangle (canvasSize.w - batSize.w) s.p2.pos batSize.w batSize.h)
      pure unit
    Nothing -> pure unit
      


type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}
