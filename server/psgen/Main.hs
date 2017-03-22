{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Main where 

import WebApi
import PSApp
import System.BlogRepository
import Data.Wizard.Command
import Data.Wizard.View
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript
import           Servant.Subscriber.Subscribable


fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "WebApi"
  t <- view haskType
  TypeInfo (_typePackage t) "Chat.ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

commandTypes :: [SumType 'Haskell]
commandTypes = [ mkSumType (Proxy :: Proxy GameCommand)
               , mkSumType (Proxy :: Proxy Direction)
               , mkSumType (Proxy :: Proxy ConfigurationCommand)]

viewTypes :: [SumType 'Haskell]
viewTypes = [ mkSumType (Proxy :: Proxy GameView)
            , mkSumType (Proxy :: Proxy Shape)
            , mkSumType (Proxy :: Proxy PlayerView)
            , mkSumType (Proxy :: Proxy Colour)  ]
myTypes :: [SumType 'Haskell]
myTypes =  commandTypes <> viewTypes <> [
          mkSumType (Proxy :: Proxy Blog)
        , mkSumType (Proxy :: Proxy PSApp)
        , mkSumType (Proxy :: Proxy ChatMessage)
        , mkSumType (Proxy :: Proxy AuthToken)   
          ]

mySettings :: Settings
mySettings = (defaultSettings & apiModuleName .~ "WebAPI") {
  _generateSubscriberAPI = True
  }

main :: IO ()
main = do
  let frontEndRoot = "static/purescript/generated"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy appAPI
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes