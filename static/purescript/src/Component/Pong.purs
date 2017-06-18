module Component.Pong where 


import Halogen.HTML.Events as E
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Control.Monad.Aff
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (clearInterval, setInterval, IntervalId, TIMER)

import Data.Boolean (otherwise) 
import Data.Eq ((==))
import Data.Int (ceil)
import Data.Maybe (Maybe(..))  
import Data.Semigroup ((<>))
import Graphics.Canvas (CANVAS)
import Halogen (ComponentDSL, ComponentHTML, Component, SubscribeStatus(Listening), action, lifecycleComponent, get, modify, subscribe, eventSource_, EventSource)
import Pong (sendCommand, PongState(..), DirY(..), Move(..), Player(..), PongCommand(..), renderPong, initialPongState)
import Prelude
import Control.Monad.Aff.AVar(AVAR)
import DOM.Event.KeyboardEvent as K

type State = { pong :: PongState, loop :: Maybe IntervalId}

initial :: State
initial = {pong: initialPongState, loop: Nothing}
type KeyCode = Number
data Query a = NewGame a | StopGame a | Input K.KeyboardEvent a | StepGame a

type PongEffects eff = (canvas :: CANVAS , timer :: TIMER, console :: CONSOLE, avar :: AVAR | eff)

game :: forall eff. Component H.HTML Query Unit Void (Aff (PongEffects eff))
game = lifecycleComponent {initialState: const initial, receiver: const Nothing, render, eval, initializer: Just (action NewGame), finalizer: Just (action StopGame)}
            where
              render :: State -> ComponentHTML Query
              render {pong} = H.div [] [ H.canvas [ P.id_ canvasName
                                             , E.onKeyDown  (E.input (\ ke-> Input ke))
                                             , P.tabIndex 0
                                             , P.height $ ceil canvasSize.h
                                             , P.width
                                               $ ceil canvasSize.w ]
                                        , renderGameInfo pong]

              eval :: Query ~> (ComponentDSL State Query Void (Aff (PongEffects eff)))
              eval (NewGame a) = do
                s <- get
                liftEff $ log "initialise pong"
                liftEff $ renderPong canvasName s.pong
                subscribe $ stepper
                pure a
              eval (Input ke a) =
                case lookupCommand (K.code ke) of
                  Nothing -> pure a
                  Just com -> do
                    modify (\s -> s {pong = sendCommand (com) s.pong})
                    s <- get
                    liftEff $ renderPong canvasName s.pong
                    pure a
              eval (StopGame a) = do
                s <- get
                case s.loop of
                  (Just i) -> do
                    liftEff $ log "clear pong"
                    liftEff $ clearInterval i
                    pure a
                  Nothing -> pure a
                pure a
              eval (StepGame a) = do
                modify (\s -> s { pong = sendCommand Step s.pong })
                s <- get
                liftEff $ renderPong canvasName s.pong
                pure a

lookupCommand :: String -> Maybe PongCommand
lookupCommand c | c == "a" = Just $ MovePlayer { player: One, move: Direction Down}
                | c == "z" = Just $ MovePlayer { player: One, move: Direction Up}
                | c == "j" = Just $ MovePlayer { player: Two, move: Direction Down}
                | c == "m" = Just $ MovePlayer { player: Two, move: Direction Up}
                | otherwise = Nothing
renderGameInfo :: PongState -> ComponentHTML Query 
renderGameInfo (PongState {score:{one, two} }) = H.div [] [ H.text (show one <> " : " <> show two) 
                                                          , H.p [] [H.text "Player One: 'w' and 's' keys"]
                                                          , H.p [] [H.text "Player Two: 'up' and 'down' keys"]]
 
canvasName :: String
canvasName = "Foo"

type Dimension = {h :: Number, w :: Number}

canvasSize :: Dimension
canvasSize = {h: 300.0, w: 500.0}

batSize :: Dimension
batSize = {h: 70.0, w: 10.0}

loop :: forall eff . Eff (timer :: TIMER | eff) Unit -> Eff (timer:: TIMER | eff) Unit   
loop a = let x = void $ setInterval 50 a in
  x

stepper ::  forall eff. EventSource Query (Aff (PongEffects eff))
stepper = eventSource_ loop (StepGame Listening)
