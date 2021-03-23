{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse.Dijkstra where

import Control.Monad
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Graph.Sparse
import Data.Heap.Binary

dijkstraCSR ::
  (U.Unbox w, Num w, Ord w, Bounded w) =>
  Vertex ->
  SparseGraph w ->
  U.Vector w
dijkstraCSR source gr@CSR{..} = U.create $ do
  dist <- UM.replicate numVerticesCSR maxBound
  heap <- newMinBinaryHeap numEdgesCSR
  UM.write dist source 0
  insertBH (0, source) heap
  fix $ \loop -> do
    deleteFindTopBH heap >>= \case
      Just (d, v) -> do
        dv <- UM.unsafeRead dist v
        when (dv == d) $ do
          U.forM_ (gr `adjW` v) $ \(nv, w) -> do
            dnv <- UM.unsafeRead dist nv
            when (dv + w < dnv) $ do
              UM.unsafeWrite dist nv $ dv + w
              insertBH (dv + w, nv) heap
        loop
      Nothing -> return ()
  return dist
