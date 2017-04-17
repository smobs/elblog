{-# LANGUAGE Arrows #-}
module GameWire where

import           Data.IORef
import Control.Wire.Core
import Control.Wire
import FRP.Netwire
import Data.Wizard
import qualified Data.Wizard.Command as Com
import Prelude hiding ((.), id)
import Control.Concurrent.STM

type Input = Integer
type Output = (Input, String)


gameSystem :: IO Input -> (Output -> IO ()) -> IO ()
gameSystem i o = testWireM id (clockSession_)  $ gameWire i o

gameWire ::  (Show t, HasTime t s) => IO Input -> (Output -> IO ()) -> Wire s () IO b ()
gameWire inIO outIO = proc i -> do
    x <- mkGen_ (\_ -> Right <$> inIO) -< i
    out <- systemWire -< x
    mkGen_ (\y -> Right <$> outIO y) -< out

systemWire :: (Show t, HasTime t s, Monad m, Monoid e) => Wire s e m Input Output
systemWire = proc i -> do
    s <- time -< ()
    ev <- periodic 3 -< i
    x <- asSoonAs -< (+ 1) <$> ev
    returnA -< (x, show s)

run :: TVar Input -> IO ()
run ref = gameSystem (atomically $ readTVar ref) (\(x, s) -> do 
    atomically $ writeTVar ref x
    print x
    print s)
