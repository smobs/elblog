module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E


main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI myComponent {on: false}
  onLoad $ appendToBody app.node

-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall g. (Functor g) => Component State Query g
myComponent = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState continue) = do
    value <- gets _.on
    pure (continue value)
