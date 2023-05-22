{-# LANGUAGE LambdaCase #-}

module Data.Graph.Tree.DFS where

import Control.Monad
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Buffer
import Data.Graph.Sparse

shortestPath :: (U.Unbox w, Num w) => SparseGraph w -> Vertex -> U.Vector w
shortestPath gr root = U.create $ do
  let n = numVerticesSG gr
  dist <- UM.unsafeNew n
  UM.unsafeWrite dist root 0
  stack <- newBufferAsStack n
  parent <- UM.unsafeNew n

  U.forM_ (gr `iadjW` root) $ \(ei, v, d) -> do
    pushBack ei stack
    UM.unsafeWrite parent v root
    UM.unsafeWrite dist v d

  fix $ \loop ->
    popBack stack >>= \case
      Just ei -> do
        let v = adjacentSG gr `U.unsafeIndex` ei
        pv <- UM.unsafeRead parent v
        dv <- UM.unsafeRead dist v
        U.forM_ (gr `iadjW` v) $ \(nei, nv, d) -> do
          when (pv /= nv) $ do
            pushBack nei stack
            UM.unsafeWrite parent nv v
            UM.unsafeWrite dist nv $ dv + d
        loop
      Nothing -> return ()
  return dist

diameter :: (U.Unbox w, Ord w, Num w) => SparseGraph w -> w
diameter tree =
  U.maximum
    . shortestPath tree
    . U.maxIndex
    $ shortestPath tree 0

height :: (U.Unbox w, Ord w, Num w) => SparseGraph w -> U.Vector w
height tree = U.zipWith max fromS fromT
  where
    s = U.maxIndex $ shortestPath tree 0
    fromS = shortestPath tree s
    t = U.maxIndex fromS
    fromT = shortestPath tree t
