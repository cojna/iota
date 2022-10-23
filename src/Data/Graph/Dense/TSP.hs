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

{- |

/O(n^2)/

>>> let gr = fromListDG @Int [[0,1,999],[999,0,2],[4,999,0]]
>>> reconstructTSP gr $ runTSP gr
[2,0,1,2]
-}
reconstructTSP ::
  (U.Unbox w, Num w, Eq w) =>
  DenseGraph w ->
  TSPResult w ->
  U.Vector Int
reconstructTSP
  gr@DenseGraph{numVerticesDG = n}
  TSPResult
    { freezedTSP = dp
    , lastVisitTSP = lastVisit
    , resultTSP
    } = U.create $ do
    path <- UM.unsafeNew (n + 1)
    UM.write path 0 lastVisit
    UM.write path n lastVisit

    U.foldM'_
      ( \(!visited, !nv, !dnv) pos -> do
          let !v =
                maybe (error "reconstructTSP") id
                  . U.findIndex (isPrev visited nv dnv)
                  $ U.generate n id
          UM.write path pos v
          pure (deleteBS v visited, v, dist visited v)
      )
      (deleteBS lastVisit visitedAll, lastVisit, resultTSP)
      (U.generate (n - 1) ((n - 1) -))
    return path
    where
      visitedAll = BitSet (shiftL 1 n - 1)

      isPrev visited nv dnv = \v ->
        memberBS v visited && dist visited v + matDG gr v nv == dnv
      {-# INLINE isPrev #-}

      dist visited v = U.unsafeIndex dp (coerce @BitSet visited * n + v)
      {-# INLINE dist #-}
