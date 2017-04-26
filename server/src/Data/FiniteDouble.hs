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
    packBoundedDouble,
    toBoundedDouble,
    clampedAdd,
    finiteZero
    ) where

import GHC.TypeLits
import Data.Maybe (fromJust)
import Data.Singletons
import Data.Singletons.TypeLits

newtype FiniteDouble (n :: Nat) = FiniteDouble Double deriving (Eq, Show)

data BoundedDouble where 
   BDouble :: (FiniteDouble n) -> BoundedDouble

instance Show BoundedDouble where 
    show (BDouble x) =  show x

getFinite :: FiniteDouble n -> Double
getFinite (FiniteDouble d) = d

boundsTest :: Double -> Integer -> Bool
boundsTest x i = (ceiling x) <= i && x >= 0

finiteZero :: KnownNat n => FiniteDouble n
finiteZero = FiniteDouble 0

unsafePackFinite :: KnownNat n => Double -> FiniteDouble n
unsafePackFinite x = result
    where 
        result = if boundsTest x (natVal result)
                 then FiniteDouble x
                 else error "out of bounds"

packFinite :: KnownNat n => Double -> Maybe (FiniteDouble n)
packFinite x = result
    where 
        result = if boundsTest x (natVal $ fromJust result)
                 then Just (FiniteDouble x)
                 else Nothing

packSizedFinite' ::  Sing n -> Double -> Maybe (FiniteDouble n)
packSizedFinite' SNat x = (packFinite x) 

packBoundedDouble :: Integer -> Double -> Maybe (BoundedDouble)
packBoundedDouble i x = case (toSing i) of
        SomeSing s -> BDouble <$> (packSizedFinite' s x)

toBoundedDouble :: Double -> BoundedDouble
toBoundedDouble d = fromJust $ packBoundedDouble (ceiling d) d 


clampedAdd :: KnownNat n => FiniteDouble n -> Double -> FiniteDouble n
clampedAdd f d = let bd = toBoundedDouble $ abs d 
                 in if d < 0 
                    then clampedSubtract f bd
                    else clampedAdd' f bd

clampedAdd' :: KnownNat n => FiniteDouble n -> BoundedDouble -> FiniteDouble n
clampedAdd' f@(FiniteDouble x) (BDouble (FiniteDouble y)) = case packFinite (x + y) of
     Nothing -> unsafePackFinite $ fromIntegral $ natVal f
     Just s -> s
    
clampedSubtract :: KnownNat n => FiniteDouble n -> BoundedDouble -> FiniteDouble n
clampedSubtract (FiniteDouble x) (BDouble (FiniteDouble y)) = case packFinite (x - y) of
     Nothing -> unsafePackFinite 0
     Just s -> s


