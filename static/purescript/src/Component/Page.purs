module Component.Page where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Core (className, ClassName)
import Halogen.HTML.CSS.PureCSS as Pure

import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))

import Control.Monad.Aff.Class
import Control.Monad.Aff.Free

import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct())
import Data.Tuple

import HTML.Components as C

import Component.Blog as Blog
import Component.About as About
import Component.Pong as Pong

import Model

type State = Page
type PageEffects a = Pong.PongEffects(Blog.BlogEffects a)

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


type ChildState g = Either (Blog.FState g) (Either About.State Pong.State)
type ChildQuery = Coproduct (Blog.FQuery) (Coproduct About.Query Pong.Query)
type ChildSlot = Either BlogSlot (Either AboutSlot PongSlot)

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
    
    pathToBlog :: ChildPath (Blog.FState a) (ChildState a) Blog.FQuery ChildQuery BlogSlot ChildSlot
    pathToBlog = cpL

    pathToAbout :: ChildPath About.State (ChildState a) About.Query ChildQuery AboutSlot ChildSlot
    pathToAbout = cpR :> cpL

    pathToPong :: ChildPath Pong.State (ChildState a) Pong.Query ChildQuery PongSlot ChildSlot
    pathToPong = cpR :> cpR
    
    renderLinks :: forall p i . HTML p i
    renderLinks = C.nav "TOBY'S BLOG"
                  [ Tuple "PONG" "/#/pong"
                  , Tuple "ABOUT" "/#/about"
                  ]
