module Pong where

import Prelude
import CSS (green)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (clearRect, getContext2D, getCanvasElementById, CANVAS)
import Graphics.Drawing (fillColor, Drawing, render, rectangle, filled, black)

type BatState = {pos :: Number, dir :: Move}

type BallState = { x :: Number, y :: Number, dir :: BallDir }

data DirX = Left | Right
data DirY = Up | Down
data BallDir = BallDir DirX DirY

newtype PongState = PongState { p1 :: BatState
                              , p2 :: BatState
                              , ball :: BallState}

data Player = One | Two
data Move = Direction DirY | Stop
data PongCommand = MovePlayer {player :: Player, move :: Move} | Step

initial :: PongState
initial = PongState {p1: {pos:0.0, dir: Stop}, p2: {pos:0.0, dir: Stop}, ball: {x: 10.0, y: 10.0, dir: BallDir Left Down}}

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
    Direction Up -> bs {pos = pos - 1.0}
    Direction Down -> bs {pos = pos + 1.0}
    
renderPong :: String -> PongState -> Eff (canvas :: CANVAS) Unit
renderPong id (PongState s) =  do
  mCanvas <- getCanvasElementById id
  case mCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      clearRect ctx {x: 0.0, y: 0.0, w: canvasSize.w, h: canvasSize.h} 
      render ctx $ batDrawing 0.0 s.p1.pos
      render ctx $ batDrawing (canvasSize.w - batSize.w) s.p2.pos
      render ctx $ ballDrawing s.ball
      pure unit
    Nothing -> pure unit

batDrawing :: Number -> Number -> Drawing
batDrawing x y = filled (fillColor black) (rectangle x y batSize.w batSize.h)

ballDrawing :: BallState -> Drawing
ballDrawing {x, y} = filled (fillColor green) (rectangle x y ballSize.w ballSize.h)

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}

ballSize :: Dimension
ballSize = {h: 20.0, w: 20.0}
