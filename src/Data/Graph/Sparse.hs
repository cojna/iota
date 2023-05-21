{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse where

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

--
import Data.Buffer

type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
type EdgeId = Int
data SparseGraph w = CSR
  { numVerticesCSR :: !Int
  , numEdgesCSR :: !Int
  , offsetCSR :: !(U.Vector Int)
  , adjacentCSR :: !(U.Vector Vertex)
  , edgeCtxCSR :: !(U.Vector w)
  }

data SparseGraphBuilder s w = SparseGraphBuilder
  { numVerticesSGB :: !Int
  , bufferSGB :: Buffer s (EdgeWith w)
  , outDegSGB :: UM.MVector s Int
  }

buildSparseGraph ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of edges
  Int ->
  (forall s. SparseGraphBuilder s w -> ST s ()) ->
  SparseGraph w
buildSparseGraph numVerticesCSR ubNumE run = runST $ do
  bufferSGB <- newBuffer ubNumE
  outDegSGB <- UM.replicate numVerticesCSR 0
  run SparseGraphBuilder{numVerticesSGB = numVerticesCSR, ..}
  numEdgesCSR <- lengthBuffer bufferSGB
  offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB
  moffset <- U.thaw offsetCSR
  madj <- UM.unsafeNew numEdgesCSR
  mectx <- UM.unsafeNew numEdgesCSR
  edges <- unsafeFreezeBuffer bufferSGB
  U.forM_ edges $ \(src, dst, w) -> do
    pos <- UM.unsafeRead moffset src
    UM.unsafeWrite moffset src (pos + 1)
    UM.unsafeWrite madj pos dst
    UM.unsafeWrite mectx pos w
  adjacentCSR <- U.unsafeFreeze madj
  edgeCtxCSR <- U.unsafeFreeze mectx
  return CSR{..}
{-# INLINE buildSparseGraph #-}

addDirectedEdge ::
  (U.Unbox w, PrimMonad m) =>
  SparseGraphBuilder (PrimState m) w ->
  EdgeWith w ->
  m ()
addDirectedEdge SparseGraphBuilder{..} (src, dst, w) = do
  pushBack (src, dst, w) bufferSGB
  UM.unsafeModify outDegSGB (+ 1) src
{-# INLINE addDirectedEdge #-}

addUndirectedEdge ::
  (U.Unbox w, PrimMonad m) =>
  SparseGraphBuilder (PrimState m) w ->
  EdgeWith w ->
  m ()
addUndirectedEdge SparseGraphBuilder{..} (src, dst, w) = do
  pushBack (src, dst, w) bufferSGB
  pushBack (dst, src, w) bufferSGB
  UM.unsafeModify outDegSGB (+ 1) src
  UM.unsafeModify outDegSGB (+ 1) dst
{-# INLINE addUndirectedEdge #-}

addDirectedEdge_ ::
  (PrimMonad m) =>
  SparseGraphBuilder (PrimState m) () ->
  Edge ->
  m ()
addDirectedEdge_ SparseGraphBuilder{..} (src, dst) = do
  pushBack (src, dst, ()) bufferSGB
  UM.unsafeModify outDegSGB (+ 1) src
{-# INLINE addDirectedEdge_ #-}

addUndirectedEdge_ ::
  (PrimMonad m) =>
  SparseGraphBuilder (PrimState m) () ->
  Edge ->
  m ()
addUndirectedEdge_ SparseGraphBuilder{..} (src, dst) = do
  pushBack (src, dst, ()) bufferSGB
  pushBack (dst, src, ()) bufferSGB
  UM.unsafeModify outDegSGB (+ 1) src
  UM.unsafeModify outDegSGB (+ 1) dst
{-# INLINE addUndirectedEdge_ #-}

buildDirectedGraph ::
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of edges
  Int ->
  U.Vector Edge ->
  SparseGraph ()
buildDirectedGraph numVerticesCSR ubNumE edges =
  buildSparseGraph numVerticesCSR ubNumE $ \builder -> do
    U.mapM_ (addDirectedEdge_ builder) edges

{- |
>>> numEdgesCSR . buildUndirectedGraph 2 1 $ U.fromList [(0, 1)]
2
-}
buildUndirectedGraph ::
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of undirected edges
  Int ->
  U.Vector Edge ->
  SparseGraph ()
buildUndirectedGraph numVerticesCSR ubNumE edges =
  buildSparseGraph numVerticesCSR (2 * ubNumE) $ \builder -> do
    U.mapM_ (addUndirectedEdge_ builder) edges

buildDirectedGraphW ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of edges
  Int ->
  U.Vector (EdgeWith w) ->
  SparseGraph w
buildDirectedGraphW numVerticesCSR ubNumE edges =
  buildSparseGraph numVerticesCSR ubNumE $ \builder -> do
    U.mapM_ (addDirectedEdge builder) edges

buildUndirectedGraphW ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of undirected edges
  Int ->
  U.Vector (EdgeWith w) ->
  SparseGraph w
buildUndirectedGraphW numVerticesCSR ubNumE edges =
  buildSparseGraph numVerticesCSR (2 * ubNumE) $ \builder -> do
    U.mapM_ (addUndirectedEdge builder) edges

adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adj #-}

iadj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
iadj CSR{..} v = U.imap ((,) . (+ o)) $ U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadj #-}

adjW ::
  (U.Unbox w) =>
  SparseGraph w ->
  Vertex ->
  U.Vector (Vertex, w)
adjW CSR{..} v =
  U.zip
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adjW #-}

iadjW ::
  (U.Unbox w) =>
  SparseGraph w ->
  Vertex ->
  U.Vector (EdgeId, Vertex, w)
iadjW CSR{..} v =
  U.izipWith
    (\i u w -> (i + o, u, w))
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadjW #-}

outEdges :: SparseGraph w -> Vertex -> U.Vector EdgeId
outEdges CSR{..} v = U.generate (o' - o) (+ o)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE outEdges #-}

outDegree :: SparseGraph w -> Vertex -> Int
outDegree CSR{..} v =
  U.unsafeIndex offsetCSR (v + 1)
    - U.unsafeIndex offsetCSR v
{-# INLINE outDegree #-}

outDegrees :: SparseGraph w -> U.Vector Int
outDegrees CSR{..} = U.zipWith (-) (U.tail offsetCSR) offsetCSR
{-# INLINE outDegrees #-}
