{-# LANGUAGE CPP, LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse.BFS where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.VecQueue

#define INF 0x3f3f3f3f3f3f3f3f

bfsCSR :: Vertex -> SparseGraph w -> U.Vector Int
bfsCSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR INF
    que <- newVecQueue numEdgesCSR
    UM.write dist source 0
    enqueue source que
    fix $ \loop -> do
        dequeue que >>= \case
            Just v -> do
                dv <- UM.unsafeRead dist v
                U.forM_ (gr `adj` v) $ \v' -> do
                    visited <- (< INF) <$> UM.unsafeRead dist v'
                    unless visited $ do
                        UM.unsafeWrite dist v' $ dv + 1
                        enqueue v' que
                loop
            Nothing -> return ()
    return dist
