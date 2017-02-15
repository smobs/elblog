module Component.Pong where 

import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import CSS (a)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (clearInterval, interval, Interval, TIMER)
import DOM.HTML.HTMLElement (offsetHeight)
import Data.Boolean (otherwise)
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Graphics.Canvas (CANVAS)
import Halogen (ComponentDSL, ComponentHTML, Component, HalogenEffects, action, lifecycleComponent, liftH, get, modify, subscribe, eventSource_, EventSource)
import Halogen.HTML.Properties (pixels)
import Pong (sendCommand, PongState(..), DirY(..), Move(..), Player(..), PongCommand(..), renderPong, initialPongState)
import Prelude (class Monad, type (~>), Unit, bind, pure, show, void, ($), (+), (<<<))


type State = { pong :: PongState, loop :: Maybe Interval}

initial :: State
initial = {pong: initialPongState, loop: Nothing}
type KeyCode = Number
data Query a = NewGame a | StopGame a | Input KeyCode a | StepGame a

type PongEffects eff = (canvas :: CANVAS , timer :: TIMER, console :: CONSOLE | eff)

game :: forall g eff. (Monad g, Affable (HalogenEffects(PongEffects eff)) g, MonadEff (HalogenEffects(PongEffects eff)) g) => Component State Query g 
game = lifecycleComponent {render, eval, initializer: Just (action NewGame), finalizer: Just (action StopGame)}
            where
              render :: State -> ComponentHTML Query
              render {pong} = H.div [] [ H.canvas [ P.id_ canvasName
                                             , E.onKeyDown  (E.input (\ {keyCode} -> Input keyCode))
                                             , P.tabIndex 0
                                             , P.height $ pixels $ ceil canvasSize.h
                                             , P.width
                                               $ pixels
                                               $ ceil canvasSize.w ]
                                        , renderGameInfo pong]

              eval :: Query ~> (ComponentDSL State Query g)
              eval (NewGame a) = do
                s <- get
                liftH <<< liftEff $ log "initialise pong"
                liftH <<< liftEff $ renderPong canvasName s.pong
                subscribe $ stepper
                pure a
              eval (Input keyCode a) =
                case lookupCommand keyCode of
                  Nothing -> pure a
                  Just com -> do
                    modify (\s -> s {pong = sendCommand (com) s.pong})
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
                modify (\s -> s { pong = sendCommand Step s.pong })
                s <- get
                liftH <<< liftEff $ renderPong canvasName s.pong
                pure a

lookupCommand :: KeyCode -> Maybe PongCommand
lookupCommand c | otherwise = Just $ MovePlayer { player: One, move: Direction Down}

renderGameInfo :: PongState -> ComponentHTML Query 
renderGameInfo (PongState {score:{one, two} }) = H.div [] [H.text (show one <> " : " <> show two) ]

canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}

loop :: forall eff . Eff (timer :: TIMER | eff) Unit -> Eff (timer:: TIMER | eff) Unit   
loop a = let x = void $ interval 100 a in
  x

stepper ::  forall g eff. (Monad g, Affable (HalogenEffects(PongEffects eff)) g) => EventSource Query g
stepper = eventSource_ loop (pure $ action StepGame)
