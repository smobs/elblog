module Component.Page where

import Prelude
import Halogen
import Control.Monad.Aff.Class
import Control.Monad.Aff.Free
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
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Core (className, ClassName)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import WebSocket (WEBSOCKET)

type State = Page
type PageEffects a =  ( channel :: CHANNEL
                      , "ref" :: REF
                      , "ws" :: WEBSOCKET
                      , ajax :: AJAX
                      , canvas :: CANVAS
                      , console :: CONSOLE
                      , timer :: TIMER | a)

data Query a =
  Navigate Page a

data BlogSlot = BlogSlot

instance ordBlogSlot :: Ord BlogSlot where
  compare _ _ = EQ

instance eqBlogSlot :: Eq BlogSlot where
  eq _ _ = true

data AboutSlot = AboutSlot
instance ordAboutSlot :: Ord AboutSlot where
  compare _ _ = EQ

instance eqAboutSlot :: Eq AboutSlot where
  eq _ _ = true

data PongSlot = PongSlot
instance ordPongSlot :: Ord PongSlot where
  compare _ _ = EQ

instance eqPongSlot :: Eq PongSlot where
  eq _ _ = true

data ChatSlot = ChatSlot
instance ordChatSlot :: Ord ChatSlot where
  compare _ _ = EQ

instance eqChatSlot :: Eq ChatSlot where
  eq _ _ = true

data GameSlot = GameSlot
instance ordGameSlot :: Ord GameSlot where
  compare _ _ = EQ

instance eqGameSlot :: Eq GameSlot where
  eq _ _ = true
  
type ChildState g = Either (Blog.FState g) (Either About.State (Either Pong.State (Either Chat.State Game.State)))
type ChildQuery = Coproduct (Blog.FQuery) (Coproduct About.Query (Coproduct Pong.Query (Coproduct Chat.Query Game.Query)))
type ChildSlot = Either BlogSlot (Either AboutSlot (Either PongSlot (Either ChatSlot GameSlot)))

type FState g = ParentState State (ChildState g) Query ChildQuery g ChildSlot
type FQuery = Coproduct Query (ChildF ChildSlot ChildQuery)
type PageDSL g = ParentDSL State (ChildState g) Query ChildQuery g ChildSlot
type PageHTML g = ParentHTML (ChildState g) Query ChildQuery g ChildSlot

page :: forall a eff . (Functor a, MonadAff (HalogenEffects(PageEffects eff)) a, Affable (HalogenEffects(PageEffects eff)) a) => Component (FState a) FQuery a
page =
  parentComponent {render, eval, peek: Nothing}
  where
    render :: State -> PageHTML a
    render s = H.div_
               [ renderLinks
               , H.div [P.class_ Pure.grid]
                 [H.div [P.class_ $ Pure.u 1 24] []
                 , H.div [P.class_ $ Pure.u 22 24] [renderPage s]
                 , H.div [P.class_ $ Pure.u 1 24] []]]

    eval :: Query ~> (PageDSL a)
    eval (Navigate p a) = do
      modify (\_ -> p)
      pure a

    renderPage :: State -> PageHTML a
    renderPage BlogPage = H.slot' pathToBlog BlogSlot (\_ -> {initialState: parentState initialBlog, component: Blog.blog})
    renderPage AboutPage = H.slot' pathToAbout AboutSlot (\_ -> {initialState: unit, component: About.about})
    renderPage PongPage = H.slot' pathToPong PongSlot (\_ -> {initialState: Pong.initial, component: Pong.game})
    renderPage ChatPage = H.slot' pathToChat ChatSlot (\_ -> {initialState: Chat.initial, component: Chat.chat})
    renderPage GamePage = H.slot' pathToGame GameSlot (\_ -> {initialState: Game.initial, component: Game.game})

    pathToBlog :: ChildPath (Blog.FState a) (ChildState a) Blog.FQuery ChildQuery BlogSlot ChildSlot
    pathToBlog = cpL

    pathToAbout :: ChildPath About.State (ChildState a) About.Query ChildQuery AboutSlot ChildSlot
    pathToAbout = cpR :> cpL

    pathToPong :: ChildPath Pong.State (ChildState a) Pong.Query ChildQuery PongSlot ChildSlot
    pathToPong = cpR :> cpR :> cpL

    pathToChat :: ChildPath Chat.State (ChildState a) Chat.Query ChildQuery ChatSlot ChildSlot
    pathToChat = cpR :> cpR :> cpR :> cpL

    pathToGame :: ChildPath Game.State (ChildState a) Game.Query ChildQuery GameSlot ChildSlot
    pathToGame = cpR :> cpR :> cpR :> cpR

    renderLinks :: forall p i . HTML p i
    renderLinks = C.nav "TOBY'S BLOG"
                  [ Tuple "PONG" "/#/pong"
                  , Tuple "CHAT" "/#/chat"
                  , Tuple "GAME" "/#/game"
                  , Tuple "ABOUT" "/#/about"
                  ]
