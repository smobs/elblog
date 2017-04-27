{-# LANGUAGE Arrows #-}
module GameWire where

import Data.Wizard
import Data.Wizard.View
import qualified Data.Wizard.Command as Com
import Prelude hiding ((.), id)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time

type Input = [(PlayerId, Com.GameCommand)]
type Output = GameView


gameSystem :: TVar Input -> (Output -> IO ()) -> IO ()
gameSystem ref o = do 
    t <- getCurrentTime
    i <- initialState
    gameLoop ref o i t


readAndEmpty :: Monoid a => TVar a -> STM a
readAndEmpty ref = do
    x <- readTVar ref
    writeTVar ref mempty
    pure x

gameLoop :: TVar Input -> (Output -> IO ()) -> GameState -> UTCTime -> IO ()
gameLoop ref out g t = do
    t' <- getCurrentTime
    let delta = diffUTCTime t' t
    actions <- liftIO ( atomically  (readAndEmpty ref))
    g' <- stepGame (fromRational (toRational delta)) $ foldr (\(pid, c) g -> updateGame pid c g) g actions
    out $ stateToGameView g' 
    threadDelay (1000000  `div` 30)
    gameLoop ref out g' t' 