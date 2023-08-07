{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.BipartiteMatching where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--
import Data.Buffer
import My.Prelude (rep)

type Vertex = Int

bipartiteMatching ::
  -- | number of vertices
  Int ->
  (forall s. BipartiteMatchingBuilder s -> ST s ()) ->
  Int
bipartiteMatching n run = runST $ do
  builder <- newBipartiteMatchingBuilder n
  run builder
  buildBipartiteMatching builder >>= runBipartiteMatching

data BipartiteMatching s = BipartiteMatching
  { numVerticesBM :: !Int
  , matchBM :: !(UM.MVector s Int)
  , usedBM :: !(UM.MVector s Bool)
  , offsetBM :: !(U.Vector Int)
  , adjacentBM :: !(U.Vector Int)
  }

nothingBM :: Int
nothingBM = -1
{-# INLINE nothingBM #-}

dfsBM :: (PrimMonad m) => BipartiteMatching (PrimState m) -> Vertex -> (Bool -> m ()) -> m ()
dfsBM BipartiteMatching{..} = dfs
  where
    dfs !v k =
      UM.unsafeRead usedBM v >>= \case
        True -> k False
        False -> do
          UM.unsafeWrite usedBM v True
          let begin = U.unsafeIndex offsetBM v
          let end = U.unsafeIndex offsetBM (v + 1)
          flip fix begin $ \loop !i -> do
            if i < end
              then do
                let nv = U.unsafeIndex adjacentBM i
                mnv <- UM.unsafeRead matchBM nv
                if mnv == nothingBM
                  then do
                    UM.unsafeWrite matchBM v nv
                    UM.unsafeWrite matchBM nv v
                    k True
                  else do
                    dfs mnv $ \case
                      True -> do
                        UM.unsafeWrite matchBM v nv
                        UM.unsafeWrite matchBM nv v
                        k True
                      False -> loop (i + 1)
              else k False
{-# INLINE dfsBM #-}

runBipartiteMatching ::
  (PrimMonad m) =>
  BipartiteMatching (PrimState m) ->
  m Int
runBipartiteMatching bm@BipartiteMatching{..} = do
  res <- UM.replicate 1 0
  updated <- UM.replicate 1 True
  fix $ \loop -> do
    UM.unsafeWrite updated 0 False
    rep numVerticesBM $ \i -> do
      mi <- UM.unsafeRead matchBM i
      when (mi == nothingBM) $ do
        dfsBM bm i $ \case
          True -> do
            UM.unsafeWrite updated 0 True
            UM.unsafeModify res (+ 1) 0
          False -> return ()
    UM.unsafeRead updated 0 >>= \case
      True -> do
        UM.set usedBM False
        loop
      False -> UM.unsafeRead res 0
{-# INLINE runBipartiteMatching #-}

data BipartiteMatchingBuilder s = BipartiteMatchingBuilder
  { numVerticesBMB :: !Int
  , inDegreeBMB :: UM.MVector s Int
  , edgesBMB :: Buffer s (Vertex, Vertex)
  }

newBipartiteMatchingBuilder ::
  (PrimMonad m) =>
  Int ->
  m (BipartiteMatchingBuilder (PrimState m))
newBipartiteMatchingBuilder n =
  BipartiteMatchingBuilder n
    <$> UM.replicate n 0
    <*> newBuffer (1024 * 1024)

addEdgeBMB ::
  (PrimMonad m) =>
  BipartiteMatchingBuilder (PrimState m) ->
  Vertex ->
  Vertex ->
  m ()
addEdgeBMB BipartiteMatchingBuilder{..} !src !dst = do
  UM.unsafeModify inDegreeBMB (+ 1) src
  pushBack (src, dst) edgesBMB
{-# INLINE addEdgeBMB #-}

buildBipartiteMatching ::
  (PrimMonad m) =>
  BipartiteMatchingBuilder (PrimState m) ->
  m (BipartiteMatching (PrimState m))
buildBipartiteMatching BipartiteMatchingBuilder{..} = do
  let numVerticesBM = numVerticesBMB
  matchBM <- UM.replicate numVerticesBM nothingBM
  usedBM <- UM.replicate numVerticesBM False
  offsetBM <- U.scanl' (+) 0 <$!> U.unsafeFreeze inDegreeBMB
  madjacentBM <- UM.unsafeNew (U.last offsetBM)
  moffset <- U.thaw offsetBM
  edges <- unsafeFreezeBuffer edgesBMB
  U.forM_ edges $ \(src, dst) -> do
    offset <- UM.unsafeRead moffset src
    UM.unsafeWrite moffset src (offset + 1)
    UM.unsafeWrite madjacentBM offset dst
  adjacentBM <- U.unsafeFreeze madjacentBM
  return BipartiteMatching{..}
{-# INLINE buildBipartiteMatching #-}
