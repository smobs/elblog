{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.Command where 

import GHC.Generics
import Data.Text

data Direction = Up | Down | Left | Right deriving (Generic, Eq, Show)

data ConfigurationCommand = AddPlayer Text | RemovePlayer Text deriving (Generic, Eq, Show)

data GameCommand = Move Direction | Configuration ConfigurationCommand deriving (Generic, Eq, Show)