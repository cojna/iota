{-# LANGUAGE CPP, LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse.Dijkstra where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.Heap.BinaryHeap.Min

#define INF 0x3f3f3f3f3f3f3f3f

dijkstraCSR :: (U.Unbox w, Num w, Ord w)
    => Vertex -> SparseGraph w -> U.Vector w
dijkstraCSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR INF
    heap <- newBinaryHeap numEdgesCSR
    UM.write dist source 0
    insertMinBH (0, source) heap
    fix $ \loop -> do
        deleteFindMinBH heap >>= \case
            Just (d, v) -> do
                dv <- UM.unsafeRead dist v
                when (dv == d) $ do
                    U.forM_ (gr `adjW` v) $ \(v', w') -> do
                        dv' <- UM.unsafeRead dist v'
                        when (dv + w' < dv') $ do
                            UM.unsafeWrite dist v' $ dv + w'
                            insertMinBH (dv + w', v') heap
                loop
            Nothing -> return ()
    return dist
