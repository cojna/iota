{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Data.Graph.Dense.TSP where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Unsafe.Coerce

import Data.BitSet
import My.Prelude (rep)

{- | Traveling Salesman Problem

 /O(n^2 2^n)/
-}
tsp ::
  (U.Unbox w, Num w, Ord w) =>
  -- | n
  Int ->
  -- | src
  Int ->
  -- | adjacent matrix (n x n)
  U.Vector w ->
  w
tsp n src gr = runST $ do
  dp <- UM.replicate (shiftL 1 n * n) inf
  UM.write dp (ix emptyBS src) 0
  rep (shiftL 1 n) . (. BitSet) $ \s -> do
    rep n $ \v -> do
      dpsv <- UM.unsafeRead dp (ix s v)
      rep n $ \nv -> when (notMemberBS nv s) $ do
        let !d = dpsv + U.unsafeIndex gr (v * n + nv)
        UM.unsafeModify dp (min d) (ix (insertBS nv s) nv)
  UM.read dp (ix (BitSet (shiftL 1 n - 1)) src)
  where
    ix :: BitSet -> Int -> Int
    ix i j = unsafeCoerce @BitSet @Int i * n + j
    {-# INLINE ix #-}

    !inf = U.sum $ U.replicate n (U.maximum gr)
{-# INLINE tsp #-}
