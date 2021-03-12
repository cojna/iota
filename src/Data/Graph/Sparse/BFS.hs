{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse.BFS where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.Deque

bfsCSR :: Vertex -> SparseGraph w -> U.Vector Int
bfsCSR source gr@CSR{..} = U.create $ do
    dist <- UM.replicate numVerticesCSR maxBound
    que <- newQueue numEdgesCSR
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
