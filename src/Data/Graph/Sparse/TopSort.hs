{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse.TopSort where

import           Control.Monad.ST
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.VecQueue

topSort :: SparseGraph w -> Maybe (U.Vector Int)
topSort gr = runST $ do
    let n = numVerticesCSR gr
    q <- newVecQueue n
    let inDegree = U.unsafeAccumulate (+) (U.replicate n (0 :: Int))
            . U.map (flip (,) 1)
            $ adjacentCSR gr
    U.mapM_ (flip enqueue q . fst)
        . U.filter ((== 0) . snd)
        $ U.indexed inDegree
    inDeg <- U.unsafeThaw inDegree
    fix $ \loop -> do
        dequeue q >>= \case
            Just v -> do
                U.forM_ (gr `adj` v) $ \u -> do
                    UM.unsafeRead inDeg u >>= \case
                        1 -> enqueue u q
                        i -> UM.unsafeWrite inDeg u (i - 1)
                loop
            Nothing -> return()
    res <- freezeQueueData q
    if U.length res == n
    then return $ Just res
    else return Nothing