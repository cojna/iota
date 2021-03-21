{-# LANGUAGE CPP #-}

module Math.Combinatrics where

import Control.Exception
import Data.Coerce
import Data.IntMod
import qualified Data.Vector.Unboxed as U

#define FACT_CACHE_SIZE 100100

fact :: Int -> IntMod
fact = U.unsafeIndex factCache
{-# INLINE fact #-}

recipFact :: Int -> IntMod
recipFact = U.unsafeIndex recipFactCache
{-# INLINE recipFact #-}

perm :: Int -> Int -> IntMod
perm n k = fact n * recipFact (n - k)
{-# INLINE perm #-}

comb :: Int -> Int -> IntMod
comb n k = fact n * recipFact (n - k) * recipFact k
{-# INLINE comb #-}

factCacheSize :: Int
factCacheSize = min (modulus - 1) FACT_CACHE_SIZE
{-# INLINE factCacheSize #-}

factCache :: U.Vector IntMod
factCache = U.scanl' (\x y -> x * coerce y) (1 :: IntMod) $ U.generate factCacheSize (+ 1)
{-# NOINLINE factCache #-}

recipFactCache :: U.Vector IntMod
recipFactCache =
  U.scanr' ((*) . coerce) (1 / factCache U.! factCacheSize) $
    U.generate factCacheSize (+ 1)
{-# NOINLINE recipFactCache #-}
