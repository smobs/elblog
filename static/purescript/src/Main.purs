module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Halogen
import Halogen.Util (appendToBody, onLoad)

import Model
import Component.Blog

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI blog (installedState initialBlog)
  onLoad $ appendToBody app.node

