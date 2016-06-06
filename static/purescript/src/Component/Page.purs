module Component.Page where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Core (className, ClassName)

import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))

import Control.Monad.Aff.Class

import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct())
import Data.Tuple

import Component.Blog as Blog
import Component.Jenny as Jenny
import Model

type State = Page
type PageEffects a = Blog.BlogEffects a

data Query a =
  Navigate Page a

data BlogSlot = BlogSlot

instance ordBlogSlot :: Ord BlogSlot where
  compare _ _ = EQ

instance eqBlogSlot :: Eq BlogSlot where
  eq _ _ = true

data JennySlot = JennySlot
instance ordJennySlot :: Ord JennySlot where
  compare _ _ = EQ

instance eqJennySlot :: Eq JennySlot where
  eq _ _ = true


type ChildState g = Either (Blog.FState g) Jenny.State
type ChildQuery = Coproduct (Blog.FQuery) (Jenny.Query )
type ChildSlot = Either BlogSlot JennySlot

type FState g = ParentState State (ChildState g) Query ChildQuery g ChildSlot
type FQuery = Coproduct Query (ChildF ChildSlot ChildQuery)
type PageDSL g = ParentDSL State (ChildState g) Query ChildQuery g ChildSlot
type PageHTML g = ParentHTML (ChildState g) Query ChildQuery g ChildSlot

page :: forall a eff . (Functor a, MonadAff (PageEffects eff) a) => Component (FState a) FQuery a
page =
  parentComponent {render, eval, peek: Nothing}
  where
    render :: State -> PageHTML a
    render s = H.div_
               [ renderLinks
               , H.h1_
                 [ H.text "Toby's Blog" ]
               , H.div [P.class_ pureGrid]
                 [H.div [P.class_ $ pureUnit 1 24] []
                 , H.div [P.class_ $ pureUnit 22 24] [renderPage s]
                 , H.div [P.class_ $ pureUnit 1 24] []]]

    eval :: Natural Query (PageDSL a)
    eval (Navigate p a) = do
      modify (\_ -> p)
      pure a

    renderPage :: State -> PageHTML a
    renderPage BlogPage = H.slot' pathToBlog BlogSlot (\_ -> {initialState: parentState initialBlog, component: Blog.blog})
    renderPage JennyPage = H.slot' pathToJenny JennySlot (\_ -> {initialState: unit, component: Jenny.jenny})
    
    pathToBlog :: ChildPath (Blog.FState a) (ChildState a) Blog.FQuery ChildQuery BlogSlot ChildSlot
    pathToBlog = cpL

    pathToJenny :: ChildPath Jenny.State (ChildState a) Jenny.Query ChildQuery JennySlot ChildSlot
    pathToJenny = cpR

    renderLinks :: PageHTML a
    renderLinks = makeHeader "TOBY" [ Tuple "BLOG" "/#/blog"
                  , Tuple "JENNY" "/#/jenny"
                  ]

    makeHeader :: String -> Array (Tuple String String) -> PageHTML a
    makeHeader title hs = H.div [P.classes $ map className ["pure-menu", "pure-menu-horizontal"]]
                          [ H.a [P.href "/#/", P.classes $ map className ["pure-menu-heading", "pure-menu-link"]] [H.text title]
                          , H.ul [P.class_ $ className "pure-menu-list"]
                            (map (\(Tuple n l) -> H.li [P.class_ $ className "pure-menu-item"]
                                                  [H.a [P.class_ $ className "pure-menu-link" , P.href l]
                                                    [H.text n]])
                             hs)]

    pureGrid :: ClassName
    pureGrid = className "pure-g"

    pureUnit :: Int -> Int -> ClassName
    pureUnit i n = className $ "pure-u-" ++ (show i) ++ "-" ++ (show n)
