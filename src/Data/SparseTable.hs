{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SparseTable where

import Data.Bits
import Data.Coerce
import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Math.Utils (floorLog2)

type RMQ a = SparseTable Min a

buildRMQ :: (U.Unbox a, Ord a) => U.Vector a -> RMQ a
buildRMQ = buildSparseTable
{-# INLINE buildRMQ #-}

readRMQ :: (U.Unbox a) => RMQ a -> Int -> a
readRMQ = readSparseTable
{-# INLINE readRMQ #-}

{- | min a[l..r)

 /O(1)/
-}
queryMin :: (U.Unbox a, Ord a) => RMQ a -> Int -> Int -> a
queryMin = querySparseTable
{-# INLINE queryMin #-}

newtype SparseTable (f :: * -> *) a = SparseTable
  { getSparseTable :: V.Vector (U.Vector a)
  }
  deriving (Eq, Show)

buildSparseTable ::
  forall (f :: * -> *) a.
  (U.Unbox a, Semigroup (f a), Coercible (f a) a) =>
  U.Vector a ->
  SparseTable f a
buildSparseTable vec =
  SparseTable
    . V.scanl' (\acc i -> U.zipWith (coerce ((<>) @(f a))) acc $ U.drop i acc) vec
    $ V.iterateN (floorLog2 $ U.length vec) (* 2) 1

-- | /O(1)/
readSparseTable :: (U.Unbox a) => SparseTable f a -> Int -> a
readSparseTable st = U.unsafeIndex (V.unsafeIndex (getSparseTable st) 0)

{- | append[l..r)

 /O(1)/
-}
querySparseTable ::
  forall (f :: * -> *) a.
  (U.Unbox a, Semigroup (f a), Coercible (f a) a) =>
  SparseTable f a ->
  Int ->
  Int ->
  a
querySparseTable st l r = coerce ((<>) @(f a)) x y
  where
    logStep = floorLog2 $ r - l
    row = V.unsafeIndex (getSparseTable st) logStep
    x = U.unsafeIndex row l
    y = U.unsafeIndex row $ r - unsafeShiftL 1 logStep
