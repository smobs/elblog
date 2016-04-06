module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)

import Network.HTTP.Affjax (AJAX())

import Model
import Component.Blog

main :: Eff (HalogenEffects (ajax :: AJAX)) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI blog (installedState initialBlog)
  onLoad $ appendToBody app.node

