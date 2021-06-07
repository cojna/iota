{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse.BFS01 where

import Control.Monad
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--

import Data.Deque
import Data.Graph.Sparse

bfs01CSR :: Vertex -> SparseGraph Int -> U.Vector Int
bfs01CSR source gr@CSR{..} = U.create $ do
  dist <- UM.replicate numVerticesCSR maxBound
  deque <- newDeque (numEdgesCSR + 1)
  UM.write dist source 0
  pushFront (0, source) deque
  fix $ \loop ->
    popFront deque >>= \case
      Just (dv, v) -> do
        dv' <- UM.unsafeRead dist v
        when (dv == dv') $ do
          U.forM_ (gr `adjW` v) $ \(nv, w) -> do
            dnv <- UM.unsafeRead dist nv
            when (dv + w < dnv) $ do
              UM.unsafeWrite dist nv (dv + w)
              if w == 0
                then pushFront (dv, nv) deque
                else pushBack (dv + 1, nv) deque
        loop
      Nothing -> return ()
  return dist
