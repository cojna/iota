{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse.BFS01 where

import           Control.Monad
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
--
import           Data.Graph.Sparse
import           Data.VecQueue

bfs01CSR :: Vertex -> SparseGraph Int -> U.Vector Int
bfs01CSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR maxBound
    que0 <- newVecQueue numEdgesCSR
    que1 <- newVecQueue numEdgesCSR
    UM.write dist source 0
    enqueueVQ source que0
    let dequeue = dequeueVQ que0 >>= \case
            Just v -> return $ Just v
            Nothing -> dequeueVQ que1
    fix $ \loop -> dequeue >>= \case
        Just v -> do
            dv <- UM.unsafeRead dist v
            U.forM_ (gr `adjW` v) $ \(nv, w) -> do
                dnv <- UM.unsafeRead dist nv
                when (dv + w < dnv) $ do
                    UM.unsafeWrite dist nv (dv + w)
                    if w == 0
                    then enqueueVQ nv que0
                    else enqueueVQ nv que1
            loop
        Nothing -> return ()
    return dist
