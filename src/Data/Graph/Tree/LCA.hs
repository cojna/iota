{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Data.Graph.Tree.LCA where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
--
import           Data.Graph.Sparse
import           Data.SparseTable
import           Data.VecQueue

data LCA = LCA
    { firstIndexLCA :: U.Vector Int      -- ^ first index in Euler Tour
    , rmqLCA        :: RMQ (Int, Vertex) -- ^ Euler Tour RMQ (depth, vertex)
    }

buildLCA :: (U.Unbox w) => SparseGraph w -> Vertex -> LCA
buildLCA gr root = runST $ do
    met <- newVecQueue (2 * numVerticesCSR gr - 1)
    mfv <- UM.unsafeNew (numVerticesCSR gr)

    fix (\dfs !p !d !v -> do
        lengthVQ met >>= UM.unsafeWrite mfv v
        enqueueVQ (d, v) met

        U.forM_ (gr `adj` v) $ \nv -> do
            when (nv /= p) $ do
                dfs v (d + 1) nv
                enqueueVQ (d, v) met
        ) (-1) 0 root

    eulertour <- freezeVecQueue met
    firstVisit <- U.unsafeFreeze mfv
    pure $ LCA firstVisit (buildRMQ eulertour)

-- | /O(1)/
queryLCA :: LCA -> Vertex -> Vertex -> Vertex
queryLCA LCA{..} v u
    = snd $ queryMin rmqLCA (min i j) (max i j + 1)
  where
    !i = U.unsafeIndex firstIndexLCA v
    !j = U.unsafeIndex firstIndexLCA u
{-# INLINE queryLCA #-}

-- | /O(1)/
queryDepth :: LCA -> Vertex -> Int
queryDepth LCA{..} v
    = fst . readRMQ rmqLCA $ U.unsafeIndex firstIndexLCA v
{-# INLINE queryDepth #-}
