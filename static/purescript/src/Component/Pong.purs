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

type State = Unit

data Query a = NewGame a

type PongEffects eff = (canvas :: CANVAS  | eff)

game :: forall g eff. (Functor g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Nothing}
            where
              render :: State -> ComponentHTML Query
              render _ = H.canvas [P.id_ "Foo"]
              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) = do
                liftH <<< liftEff $  draw
                pure a

draw :: forall eff. Eff (canvas :: CANVAS | eff) Int
draw = do
  Just canvas <- getCanvasElementById "Foo"
  ctx <- getContext2D canvas
  render ctx $
    filled (fillColor black) (rectangle 1.0 1.0 60.0 40.0)
  pure 1
