module Component.Page where

import Prelude
import Halogen
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar(AVAR)

import Data.Tuple
import Model
import Component.About as About
import Component.Blog as Blog
import Component.Chat as Chat
import Component.Pong as Pong
import Component.Game as Game
import HTML.Components as C
import Halogen.HTML.CSS.PureCSS as Pure
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Component.Chat (chat)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(Nothing))
import Graphics.Canvas (CANVAS)
import Halogen.Component.ChildPath as CP
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import WebSocket (WEBSOCKET)
import Data.Either.Nested
import Data.Functor.Coproduct.Nested
import DOM(DOM)
import Control.Monad.Eff.Exception (EXCEPTION)

type State = Page
type PageEffects a =  ( err :: EXCEPTION
                      , dom::DOM
                      , avar :: AVAR
                      , channel :: CHANNEL
                      , "ref" :: REF
                      , "ws" :: WEBSOCKET
                      , ajax :: AJAX
                      , canvas :: CANVAS
                      , console :: CONSOLE
                      , timer :: TIMER | a)

data Query a =
  Navigate Page a
  
type ChildQuery = Coproduct5 Blog.Query About.Query Pong.Query Chat.Query Game.Query
type ChildSlot = Either5 Unit Unit Unit Unit Unit

type PageDSL eff = ParentDSL State Query ChildQuery ChildSlot Void (Aff(PageEffects eff))
type PageHTML eff = ParentHTML Query ChildQuery ChildSlot (Aff(PageEffects eff))

page :: forall eff . Component H.HTML Query Unit Void (Aff(PageEffects eff))
page =
  parentComponent {initialState: const initialPage, receiver: const Nothing, render, eval}
  where
    render :: State -> PageHTML eff
    render s = H.div_
               [ renderLinks
               , H.div [P.class_ Pure.grid]
                 [H.div [P.class_ $ Pure.u 1 24] []
                 , H.div [P.class_ $ Pure.u 22 24] [renderPage s]
                 , H.div [P.class_ $ Pure.u 1 24] []]]

    eval :: Query ~> (PageDSL eff)
    eval (Navigate p a) = do
      modify (\_ -> p)
      pure a

    renderPage :: State -> PageHTML eff
    renderPage BlogPage = H.slot' CP.cp1 unit Blog.blog  unit absurd
    renderPage AboutPage = H.slot' CP.cp2 unit  About.about unit absurd
    renderPage PongPage = H.slot' CP.cp3 unit Pong.game unit absurd
    renderPage ChatPage = H.slot' CP.cp4 unit Chat.chat unit absurd
    renderPage GamePage = H.slot' CP.cp5 unit Game.game unit absurd

    renderLinks :: forall p i . HTML p i
    renderLinks = C.nav "TOBY'S BLOG"
                  [ Tuple "PONG" "/#/pong"
                  , Tuple "CHAT" "/#/chat"
                  , Tuple "GAME" "/#/game"
                  , Tuple "ABOUT" "/#/about"
                  ]
