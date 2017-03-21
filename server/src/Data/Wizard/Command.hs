{-# LANGUAGE DeriveGeneric #-}
module Data.Wizard.Command where 

import GHC.Generics

data Direction = Up | Down | Left | Right deriving (Generic, Eq, Show)

data GameCommand = Move Direction deriving (Generic, Eq, Show)