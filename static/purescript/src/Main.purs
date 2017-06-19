module Main where

import Prelude 
import Control.Alt ((<|>))
import Control.Monad.Aff (forkAff, Aff)
import Control.Monad.Eff (Eff())


import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Functor.Coproduct (left)
import Halogen.VDom.Driver (runUI)
import Halogen (HalogenIO, action)
import Halogen.Aff (awaitBody, runHalogenAff, HalogenEffects)

import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit)

import Model (Page(..), initialPage)
import Component.Page

main :: Eff ((PageEffects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI page unit body
  forkAff $ routeSignal driver

routeSignal :: forall eff. HalogenIO Query Void (Aff (HalogenEffects eff))
            -> Aff (HalogenEffects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. HalogenIO Query Void (Aff (HalogenEffects eff))
             -> Maybe Page
             -> Page
             -> Aff (HalogenEffects eff) Unit
redirects d _ = d.query <<< action <<< Navigate

routing :: Match Page
routing = AboutPage <$ lit "" <* lit "about"
          <|> PongPage <$ lit "" <* lit "pong"
          <|> ChatPage <$ lit "" <* lit "chat"
          <|> GamePage <$ lit "" <* lit "game"
          <|> BlogPage <$ lit ""
