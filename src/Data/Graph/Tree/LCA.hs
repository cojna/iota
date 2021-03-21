{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Tree.LCA where

import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--

import Data.Deque
import Data.Graph.Sparse
import Data.SparseTable

data LCA = LCA
  { -- | first index in Euler Tour
    firstIndexLCA :: U.Vector Int
  , -- | Euler Tour RMQ (depth, vertex)
    rmqLCA :: RMQ (Int, Vertex)
  }

buildLCA :: (U.Unbox w) => SparseGraph w -> Vertex -> LCA
buildLCA gr root = runST $ do
  met <- newBuffer (2 * numVerticesCSR gr - 1)
  mfv <- UM.unsafeNew (numVerticesCSR gr)

  fix
    ( \dfs !p !d !v -> do
        lengthBuffer met >>= UM.unsafeWrite mfv v
        pushBack (d, v) met

        U.forM_ (gr `adj` v) $ \nv -> do
          when (nv /= p) $ do
            dfs v (d + 1) nv
            pushBack (d, v) met
    )
    (-1)
    0
    root

  eulertour <- unsafeFreezeBuffer met
  firstVisit <- U.unsafeFreeze mfv
  pure $ LCA firstVisit (buildRMQ eulertour)

-- | /O(1)/
queryLCA :: LCA -> Vertex -> Vertex -> Vertex
queryLCA LCA{..} v u =
  snd $ queryMin rmqLCA (min i j) (max i j + 1)
  where
    !i = U.unsafeIndex firstIndexLCA v
    !j = U.unsafeIndex firstIndexLCA u
{-# INLINE queryLCA #-}

-- | /O(1)/
queryDepth :: LCA -> Vertex -> Int
queryDepth LCA{..} v =
  fst . readRMQ rmqLCA $ U.unsafeIndex firstIndexLCA v
{-# INLINE queryDepth #-}

-- | /O(1)/
queryDist :: LCA -> Vertex -> Vertex -> Int
queryDist lca v u =
  queryDepth lca v
    + queryDepth lca u
    - 2 * queryDepth lca (queryLCA lca v u)
{-# INLINE queryDist #-}
