{-# LANGUAGE CPP, KindSignatures, ScopedTypeVariables #-}
module Data.SparseTable where

import           Data.Bits
import           Data.Coerce
import           Data.Semigroup
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Math.Utils

type RMQ a = SparseTable Min a

buildRMQ :: (U.Unbox a, Ord a, Bounded a) => U.Vector a -> RMQ a
buildRMQ = buildSparseTable
{-# INLINE buildRMQ #-}

queryMin :: (U.Unbox a, Ord a, Bounded a) => RMQ a -> Int -> Int -> a
queryMin = queryST
{-# INLINE queryMin #-}

newtype SparseTable (f :: * -> *) a = SparseTable
    { getSparseTable :: V.Vector (U.Vector a)
    } deriving (Eq, Show)

buildSparseTable :: forall (f :: * -> *) a .
    (U.Unbox a, Monoid (f a), Coercible (f a) a)
    => U.Vector a -> SparseTable f a
buildSparseTable vec = SparseTable
    . V.scanl' (\acc i -> U.zipWith (coerce op) acc $ U.drop i acc) vec
    $ V.iterateN (floorLog2 $ U.length vec) (*2) 1
  where
    op :: f a -> f a -> f a
    op = mappend

queryST :: forall (f :: * -> *) a .
    (U.Unbox a, Monoid (f a), Coercible (f a) a)
    => SparseTable f a -> Int -> Int -> a
queryST st l r
    | l < r = (coerce op) x y
    | otherwise = error $ "queryST l: " ++ shows l "r: " ++ show r
  where
    op :: f a -> f a -> f a
    op = mappend
    logStep = floorLog2 $ r - l
    row = V.unsafeIndex (getSparseTable st) logStep
    x = U.unsafeIndex row l
    y = U.unsafeIndex row $ r - unsafeShiftL 1 logStep
