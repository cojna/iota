module Math.Combinatrics where

import           Data.IntMod.Operator
import qualified Data.Vector.Unboxed  as U

fact :: Int -> IntMod
fact = U.unsafeIndex factCache
{-# INLINE fact #-}

recipFact :: Int -> IntMod
recipFact = U.unsafeIndex recipFactCache
{-# INLINE recipFact #-}

comb :: Int -> Int -> IntMod
comb n k = fact n *% recipFact (n - k) *% recipFact k
{-# INLINE comb #-}

factCacheSize :: Int
factCacheSize = 100100
{-# INLINE factCacheSize #-}

factCache :: U.Vector IntMod
factCache = U.scanl' (*%) 1 $ U.generate factCacheSize (+1)
{-# NOINLINE factCache #-}

recipFactCache :: U.Vector IntMod
recipFactCache = U.scanr' (*%) (1 /% (factCache U.! factCacheSize))
    $ U.generate factCacheSize (+1)
{-# NOINLINE recipFactCache #-}
