module Component.Pong where 

import Prelude
import Halogen (ComponentDSL, ComponentHTML, Component, HalogenEffects, action, lifecycleComponent, liftH, get, modify)
import Data.Int (ceil)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS)
import Pong (PongState, DirY(..), Move(..), Player(..), PongCommand(..), renderPong, sendCommand, initialPongState)


type State = PongState

initial :: State
initial = initialPongState

data Query a = NewGame a | Move a

type PongEffects eff = (canvas :: CANVAS  | eff)

game :: forall g eff. (Functor g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Nothing}
            where
              render :: State -> ComponentHTML Query
              render _ = H.canvas [P.id_ canvasName
                                  , E.onKeyDown (E.input_ Move)
                                  , P.tabIndex 0
                                  , P.height $ P.Pixels $ ceil canvasSize.h
                                  , P.width
                                    $ P.Pixels
                                    $ ceil canvasSize.w ]

              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) = do
                s <- get 
                liftH <<< liftEff $ renderPong canvasName s
                pure a
              eval (Move a) = do
                modify (sendCommand (MovePlayer { player: One
                                                , move: Direction Down}))
                s <- get
                liftH <<< liftEff $ renderPong canvasName s
                pure a
                

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}
