{-# LANGUAGE Arrows #-}
module GameWire where

import           Data.IORef
import Control.Wire.Core
import Control.Wire
import FRP.Netwire
import Data.Wizard
import Data.Wizard.View
import qualified Data.Wizard.Command as Com
import Prelude hiding ((.), id)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Wire.Unsafe.Event (onEventM)

type Input = [(PlayerId, Com.GameCommand)]
type Output = GameView


gameSystem :: TVar Input -> (Output -> IO ()) -> IO ()
gameSystem ref o = testWireM id (clockSession_)  $ gameWire ref o

gameWire :: (Fractional t, MonadIO m, HasTime t s) => TVar Input ->  (Output -> IO ()) -> Wire s () m b ()
gameWire ref out = proc i -> do 
    v <- stateToGameView <$> updateWire . readAndEmptyWire ref -< i
    _ <- (onEventM $ liftIO . out) . periodic 0.1 -< v
    returnA -< ()

readAndEmptyWire :: (MonadIO m, Monoid a, Eq a) => TVar a -> Wire s e m b (Event a)
readAndEmptyWire ref = proc i -> do 
    x <- mkGen_ (\_ -> Right <$> liftIO ( atomically  (readAndEmpty ref))) -< i
    became (\y -> y /= mempty) -< x

updateWire :: (Monad m, Monoid e )=> Wire s e m (Event Input) GameState
updateWire = hold . accumE f initialState
    where f g cs = foldr (\(pid, c) g -> updateGame pid c g) g cs


readAndEmpty :: Monoid a => TVar a -> STM a
readAndEmpty ref = do
    x <- readTVar ref
    writeTVar ref mempty
    pure x

test :: TVar Input -> IO ()
test ref = do 
    gameSystem ref print
    --killThread thid