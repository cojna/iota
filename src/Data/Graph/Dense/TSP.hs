{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Data.Graph.Dense.TSP where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Coerce
import Data.Semigroup
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.BitSet
import Data.Graph.Dense
import My.Prelude (rep)

data TSPResult a = TSPResult
  { resultTSP :: !a
  , infTSP :: !a
  , lastVisitTSP :: !Int
  , freezedTSP :: U.Vector a
  }

{- | Traveling Salesman Problem

/O(n^2 2^n)/

>>> resultTSP . runTSP $ fromListDG @Int [[0,1,999],[999,0,2],[4,999,0]]
7
>>> resultTSP . runTSP $ fromListDG @Int [[0,1,1],[1,0,8],[1,999,0]]
10
>>> resultTSP . runTSP $ fromListDG @Int [[0]]
0
-}
runTSP ::
  (U.Unbox w, Num w, Ord w) =>
  DenseGraph w ->
  TSPResult w
runTSP gr@DenseGraph{numVerticesDG = n} = runST $ do
  dp <- UM.replicate (shiftL 1 n * n) inf

  -- from start
  rep (n - 1) $ \v -> do
    UM.unsafeWrite dp (ixTSP (singletonBS v) v) $ matDG gr origin v

  U.forM_ (U.generate (shiftL 1 (n - 1)) BitSet) $ \visited ->
    -- skip from goal
    rep (n - 1) $ \v -> when (memberBS v visited) $ do
      dpv <- UM.unsafeRead dp (ixTSP visited v)
      rep n $ \nv -> when (notMemberBS nv visited) $ do
        let !dnv' = dpv + matDG gr v nv
        UM.unsafeModify dp (min dnv') (ixTSP (insertBS nv visited) nv)

  !res <- UM.read dp (ixTSP visitedAll origin)
  TSPResult res inf origin
    <$> U.unsafeFreeze dp
  where
    ixTSP :: BitSet -> Int -> Int
    ixTSP i j = coerce @BitSet @Int i * n + j
    {-# INLINE ixTSP #-}

    origin = n - 1

    visitedAll = BitSet (shiftL 1 n - 1)

    !inf =
      getProduct
        . stimes n
        . Product
        . U.maximum
        $ adjacentDG gr
{-# INLINE runTSP #-}
