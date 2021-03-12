{-# LANGUAGE BangPatterns, LambdaCase #-}

module Data.Graph.Sparse.SCC where

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.Graph.Sparse
import           Data.Deque
import           My.Prelude                  (rep)

type ComponentId = Int

stronglyConnectedComponents :: SparseGraph w -> U.Vector ComponentId
stronglyConnectedComponents gr = runST $ do
    let numV = numVerticesCSR gr
    low <- UM.replicate numV nothing
    preord <- UM.replicate numV nothing
    stack <- newStack numV
    component <- UM.replicate numV nothing
    vars <- UM.replicate 2 0

    rep numV $ \root -> do
        rootOrd <- UM.unsafeRead preord root
        when (rootOrd == nothing) $ do
            flip fix root $ \dfs v -> do
                preordId <- UM.unsafeRead vars _preordId
                UM.unsafeWrite vars _preordId (preordId + 1)

                UM.unsafeWrite preord v preordId
                UM.unsafeWrite low v preordId

                pushBack v stack

                U.forM_ (adj gr v) $ \u -> do
                    ordU <- UM.unsafeRead preord u
                    if ordU == nothing
                    then do
                        dfs u
                        lowU <- UM.unsafeRead low u
                        UM.unsafeModify low (min lowU) v
                    else UM.unsafeModify low (min ordU) v

                lowV <- UM.unsafeRead low v
                ordV <- UM.unsafeRead preord v
                when (lowV == ordV) $ do
                    compId <- UM.unsafeRead vars _compId
                    fix $ \loop -> do
                        popBack stack >>= \case
                            Just x -> do
                                UM.unsafeWrite preord x numV
                                UM.unsafeWrite component x compId
                                when (x /= v) loop
                            Nothing -> undefined
                    UM.unsafeWrite vars _compId (compId + 1)
    maxCompId <- subtract 1 <$!> UM.unsafeRead vars _compId
    U.map (maxCompId -) <$> U.unsafeFreeze component
  where
    nothing = -1
    _preordId = 0
    _compId = 1