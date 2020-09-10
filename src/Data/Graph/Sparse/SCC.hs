{-# LANGUAGE BangPatterns, LambdaCase #-}

module Data.Graph.Sparse.SCC where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.VecStack
import           Utils                       (rep)


nothingSCC :: Int
nothingSCC = -1

stronglyConnectedComponents :: SparseGraph w -> [U.Vector Vertex]
stronglyConnectedComponents gr = runST $ do
    let numV = numVerticesCSR gr
    low <- UM.replicate numV nothingSCC
    preord <- UM.replicate numV nothingSCC
    stack <- newVecStack numV
    onStack <- UM.replicate numV False
    num <- newMutVar 0
    components <- newMutVar []

    rep numV $ \root -> do
        rootOrd <- UM.unsafeRead preord root
        when (rootOrd == nothingSCC) $ do
            flip fix root $ \dfs v -> do
                pord <- readMutVar num <* modifyMutVar' num (+1)
                UM.unsafeWrite preord v pord >> UM.unsafeWrite low v pord

                pushVS v stack >> UM.unsafeWrite onStack v True

                U.forM_ (adj gr v) $ \u -> do
                    isVisited <- (/= nothingSCC) <$> UM.unsafeRead preord u
                    if isVisited
                    then do
                        onS <- UM.unsafeRead onStack u
                        when onS $ do
                            uOrd <- UM.unsafeRead preord u
                            UM.unsafeModify low (min uOrd) v
                    else do
                        dfs u
                        uLow <- UM.unsafeRead low u
                        UM.unsafeModify low (min uLow) v

                isRoot <- (==)
                    <$> UM.unsafeRead low v
                    <*> UM.unsafeRead preord v
                when isRoot $ do
                    flip fix 1 $ \loop !i -> do
                        popVS stack >>= \case
                            Just x
                                | x /= v -> loop (i + 1)
                                | otherwise -> do
                                    o <- UM.unsafeRead (intVarsVS stack) _sizeVS
                                    component <- U.freeze $ UM.slice o i (internalVecStack stack)
                                    U.mapM_ (flip (UM.unsafeWrite onStack) False) component
                                    modifyMutVar' components (component:)
                            Nothing -> undefined
    readMutVar components
