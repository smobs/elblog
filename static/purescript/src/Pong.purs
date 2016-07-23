module Pong where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(Just))
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, Canvas)
import Graphics.Drawing (render, rectangle, filled, black, fillColor)

newtype PongState = PongState { p1 :: Number
                              , p2 :: Number
                              , bx :: Number
                              , by :: Number
                              , bdirX :: Number
                              , bdirY :: Number}


data Player = One | Two
data Move = Up | Down
data PongCommand = MovePlayer {player :: Player, move :: Move}

initial :: PongState
initial = PongState {p1: 0.0, p2: 0.0, bx: 40.0, by: 40.0, bdirX: 1.0, bdirY: 1.0}

sendCommand :: PongCommand -> PongState -> PongState
sendCommand (MovePlayer {player, move}) (PongState s) = PongState (
  let f = case move of
        Up -> (-)
        Down -> (+)
  in
   case player of
     One -> s {p1 = f s.p1 1.0}
     Two -> s {p2 = f s.p2 1.0})

renderPong :: String -> PongState -> Eff (canvas :: Canvas) Unit
renderPong id (PongState s) =  do
  Just canvas <- getCanvasElementById id
  ctx <- getContext2D canvas
  clearRect ctx {x: 0.0, y: 0.0, w: canvasSize.w, h: canvasSize.h} 
  render ctx $
    filled (fillColor black) (rectangle 0.0 s.p1 batSize.w batSize.h)
  render ctx $
    filled (fillColor black) (rectangle (canvasSize.w - batSize.w) s.p2 batSize.w batSize.h)
  return unit

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}
