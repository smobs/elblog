{-# LANGUAGE ScopedTypeVariables
           , TypeFamilies
           , GADTs
           , KindSignatures
           , TypeOperators
           , FlexibleContexts
           , RankNTypes
           , UndecidableInstances
           , FlexibleInstances
           , InstanceSigs
           , DefaultSignatures
           , TypeInType #-}
module Data.FiniteDouble (
    FiniteDouble(),
    getFinite,
    packFinite,
    BoundedDouble(..),
    packBoundedDouble
    ) where

import GHC.TypeLits
import Data.Maybe (fromJust)
import Data.Singletons
import Data.Singletons.TypeLits

newtype FiniteDouble (n :: Nat) = FiniteDouble Double deriving Show

data BoundedDouble where 
   BDouble :: (FiniteDouble n) -> BoundedDouble

instance Show BoundedDouble where 
    show (BDouble x) =  show x

getFinite :: FiniteDouble n -> Double
getFinite (FiniteDouble d) = d


packFinite :: KnownNat n => Double -> Maybe (FiniteDouble n)
packFinite x = result
    where 
        result = if (ceiling x) < natVal (fromJust result) && x >= 0
                 then Just (FiniteDouble x)
                 else Nothing


packSizedFinite' ::  Sing n -> Double -> Maybe (FiniteDouble n)
packSizedFinite' SNat x = (packFinite x) 

packBoundedDouble :: Integer -> Double -> Maybe (BoundedDouble)
packBoundedDouble i x = case (toSing i) of
        SomeSing s -> BDouble <$> (packSizedFinite' s x)
