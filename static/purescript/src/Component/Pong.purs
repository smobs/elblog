module Component.Pong where 

import Prelude
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Properties (pixels)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Timer (clearInterval, interval, Interval, TIMER)
import Control.Monad.Eff.Console
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS)
import Halogen (ComponentDSL, ComponentHTML, Component, HalogenEffects, action, lifecycleComponent, liftH, get, modify, subscribe, eventSource_)
import Pong (sendCommand, PongState, DirY(..), Move(..), Player(..), PongCommand(..), renderPong, initialPongState)


type State = { pong :: PongState, loop :: Maybe Interval}

initial :: State
initial = {pong: initialPongState, loop: Nothing}

data Query a = NewGame a | StopGame a | Move a | StepGame a

type PongEffects eff = (canvas :: CANVAS , timer :: TIMER, console :: CONSOLE | eff)

game :: forall g eff. (Functor g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Just (action StopGame)}
            where
              render :: State -> ComponentHTML Query
              render _ = H.canvas [P.id_ canvasName
                                  , E.onKeyDown (E.input_ Move)
                                  , P.tabIndex 0
                                  , P.height $ pixels $ ceil canvasSize.h
                                  , P.width
                                    $ pixels
                                    $ ceil canvasSize.w ]

              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) = do
                s <- get
                liftH <<< liftEff $ log "initialise pong"
                liftH <<< liftEff $ renderPong canvasName s.pong
                --subscribe $ eventSource_ (void  <<< interval 1000) (pure $ action StepGame)
                pure a
              eval (Move a) = do
                modify (\s -> s {pong = sendCommand (MovePlayer { player: One
                                                , move: Direction Down}) s.pong})
                s <- get
                liftH <<< liftEff $ renderPong canvasName s.pong
                pure a
              eval (StopGame a) = do
                s <- get
                case s.loop of
                  (Just i) -> do
                    liftH <<< liftEff $ log "clear pong"
                    liftH <<< liftEff $ clearInterval i
                    pure a
                  Nothing -> pure a
                pure a
              eval (StepGame a) = do
                modify (\s -> s {pong = sendCommand Step s.pong})
                pure a

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}

  
