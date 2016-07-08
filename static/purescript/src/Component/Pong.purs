module Component.Pong where 

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Data.Maybe(Maybe(..))
import Data.Int

import Graphics.Canvas
import Graphics.Drawing

import Control.Monad.Eff
import Control.Monad.Eff.Class

type State = {p1 :: Number, p2 :: Number, bx :: Number, by :: Number, bdirX :: Number, bdirY :: Number}

initial :: State
initial = {p1: 0.0, p2: 0.0, bx: 40.0, by: 40.0, bdirX: 1.0, bdirY: 1.0}

data Query a = NewGame a | Move a

type PongEffects eff = (canvas :: CANVAS  | eff)

game :: forall g eff. (Functor g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Nothing}
            where
              render :: State -> ComponentHTML Query
              render _ = H.canvas [P.id_ canvasName, E.onKeyDown (E.input_  Move), P.tabIndex 0, P.height $ P.Pixels $ ceil canvasSize.h, P.width $ P.Pixels $ ceil canvasSize.w ]

              eval :: Natural Query (ComponentDSL State Query g)
              eval (NewGame a) = do
                s <- get 
                liftH <<< liftEff $ draw s
                return a
              eval (Move a) = do
                modify (\s -> s {p1 = s.p1 + 1.0})
                s <- get
                liftH <<< liftEff $ draw s
                return a
                

draw :: State -> forall eff. Eff (canvas :: Canvas | eff) Unit
draw s = do
  Just canvas <- getCanvasElementById canvasName
  ctx <- getContext2D canvas
  clearRect ctx {x: 0.0, y: 0.0, w: canvasSize.w, h: canvasSize.h} 
  render ctx $
    filled (fillColor black) (rectangle 0.0 s.p1 batSize.w batSize.h)
  render ctx $
    filled (fillColor black) (rectangle (canvasSize.w - batSize.w) s.p1 batSize.w batSize.h)
  return unit

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}
