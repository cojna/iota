{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse.BFS where

import Control.Monad
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Buffer
import Data.Graph.Sparse

-- | /O(V+E)/
bfsCSR :: Vertex -> SparseGraph w -> U.Vector Int
bfsCSR source gr@CSR{..} = U.create $ do
  dist <- UM.replicate numVerticesCSR maxBound
  que <- newBufferAsQueue (numEdgesCSR + 1)
  UM.write dist source 0
  pushBack source que
  fix $ \loop -> do
    popFront que >>= \case
      Just v -> do
        dv <- UM.unsafeRead dist v
        U.forM_ (gr `adj` v) $ \nv -> do
          dnv <- UM.unsafeRead dist nv
          when (dnv == maxBound) $ do
            UM.unsafeWrite dist nv $ dv + 1
            pushBack nv que
        loop
      Nothing -> return ()
  return dist
