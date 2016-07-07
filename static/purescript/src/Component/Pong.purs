module Component.Pong where 

import Prelude
import Halogen

import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Data.Maybe(Maybe(..))

import Graphics.Canvas
import Graphics.Drawing

import Control.Monad.Eff
import Control.Monad.Eff.Class

type State = {x :: Number, y :: Number}

data Query a = NewGame a | Move a

type PongEffects eff = (canvas :: CANVAS  | eff)

game :: forall g eff. (Functor g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Nothing}
            where
              render :: State -> ComponentHTML Query
              render _ = H.canvas [P.id_ "Foo", E.onKeyDown (E.input_  Move), P.tabIndex 0 ]

              eval :: Natural Query (ComponentDSL State Query g)
              eval (NewGame a) = do
                s <- get 
                liftH <<< liftEff $ draw s
                return a
              eval (Move a) = do
                modify (\s -> s {x= s.x + 1.0, y= s.y + 1.0})
                s <- get
                liftH <<< liftEff $ draw s
                return a
                

draw :: State -> forall eff. Eff (canvas :: Canvas | eff) Int
draw s = do
  Just canvas <- getCanvasElementById "Foo"
  ctx <- getContext2D canvas
  clearRect ctx {x: 0.0, y: 0.0, w: 1000.0, h:1000.0} 
  render ctx $
    filled (fillColor black) (rectangle s.x s.y 60.0 40.0)
  return 1
