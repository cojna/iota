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
    U.mapM_ (flip enqueueVQ q . fst)
        . U.filter ((== 0) . snd)
        $ U.indexed inDegree
    inDeg <- U.unsafeThaw inDegree
    fix $ \loop -> do
        dequeueVQ q >>= \case
            Just v -> do
                U.forM_ (gr `adj` v) $ \u -> do
                    UM.unsafeRead inDeg u >>= \case
                        1 -> enqueueVQ u q
                        i -> UM.unsafeWrite inDeg u (i - 1)
                loop
            Nothing -> return ()
    enqueueCount <- UM.unsafeRead (intVarsVQ q) _enqueueCount
    if enqueueCount == n
    then Just <$> U.unsafeFreeze (internalVecQueue q)
    else return Nothing
