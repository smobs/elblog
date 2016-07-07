module Main where

import Prelude

import Control.Apply
import Control.Alt ((<|>))
import Control.Monad.Aff (runAff, forkAff, Aff(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)


import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Functor
import Data.Functor.Coproduct (left)
import Halogen
import Halogen.Util (awaitBody, runHalogenAff)

import Routing
import Routing.Match
import Routing.Match.Class

import Model
import Component.Page

main :: forall e . Eff (HalogenEffects (PageEffects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI page (parentState initialPage) body
  forkAff $ routeSignal driver

routeSignal :: forall eff. Driver FQuery eff -> Aff (HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver FQuery eff
             -> Maybe Page
             -> Page
             -> Aff (HalogenEffects eff) Unit
redirects d _ = d  <<< left <<< action <<< Navigate

routing :: Match Page
routing = AboutPage <$ lit "" <* lit "about"
          <|> PongPage <$ lit "" <* lit "pong"
          <|> BlogPage <$ lit ""
