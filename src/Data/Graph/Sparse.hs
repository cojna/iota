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

-- | CSR (Compressed Sparse Row)
data SparseGraph w = SparseGraph
  { numVerticesSG :: !Int
  , numEdgesSG :: !Int
  , offsetSG :: !(U.Vector Int)
  , adjacentSG :: !(U.Vector Vertex)
  , edgeCtxSG :: !(U.Vector w)
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
buildSparseGraph numVerticesSG ubNumE run = runST $ do
  bufferSGB <- newBuffer ubNumE
  outDegSGB <- UM.replicate numVerticesSG 0
  run SparseGraphBuilder{numVerticesSGB = numVerticesSG, ..}
  numEdgesSG <- lengthBuffer bufferSGB
  offsetSG <- U.scanl' (+) 0 <$> U.unsafeFreeze outDegSGB
  moffset <- U.thaw offsetSG
  madj <- UM.unsafeNew numEdgesSG
  mectx <- UM.unsafeNew numEdgesSG
  edges <- unsafeFreezeBuffer bufferSGB
  U.forM_ edges $ \(src, dst, w) -> do
    pos <- UM.unsafeRead moffset src
    UM.unsafeWrite moffset src (pos + 1)
    UM.unsafeWrite madj pos dst
    UM.unsafeWrite mectx pos w
  adjacentSG <- U.unsafeFreeze madj
  edgeCtxSG <- U.unsafeFreeze mectx
  return SparseGraph{..}
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
buildDirectedGraph numVerticesSG ubNumE edges =
  buildSparseGraph numVerticesSG ubNumE $ \builder -> do
    U.mapM_ (addDirectedEdge_ builder) edges

{- |
>>> numEdgesSG . buildUndirectedGraph 2 1 $ U.fromList [(0, 1)]
2
-}
buildUndirectedGraph ::
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of undirected edges
  Int ->
  U.Vector Edge ->
  SparseGraph ()
buildUndirectedGraph numVerticesSG ubNumE edges =
  buildSparseGraph numVerticesSG (2 * ubNumE) $ \builder -> do
    U.mapM_ (addUndirectedEdge_ builder) edges

buildDirectedGraphW ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of edges
  Int ->
  U.Vector (EdgeWith w) ->
  SparseGraph w
buildDirectedGraphW numVerticesSG ubNumE edges =
  buildSparseGraph numVerticesSG ubNumE $ \builder -> do
    U.mapM_ (addDirectedEdge builder) edges

buildUndirectedGraphW ::
  (U.Unbox w) =>
  -- | the number of vertices
  Int ->
  -- | upper bound on the number of undirected edges
  Int ->
  U.Vector (EdgeWith w) ->
  SparseGraph w
buildUndirectedGraphW numVerticesSG ubNumE edges =
  buildSparseGraph numVerticesSG (2 * ubNumE) $ \builder -> do
    U.mapM_ (addUndirectedEdge builder) edges

adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj SparseGraph{..} v = U.unsafeSlice o (o' - o) adjacentSG
  where
    o = U.unsafeIndex offsetSG v
    o' = U.unsafeIndex offsetSG (v + 1)
{-# INLINE adj #-}

iadj :: SparseGraph w -> Vertex -> U.Vector (EdgeId, Vertex)
iadj SparseGraph{..} v = U.imap ((,) . (+ o)) $ U.unsafeSlice o (o' - o) adjacentSG
  where
    o = U.unsafeIndex offsetSG v
    o' = U.unsafeIndex offsetSG (v + 1)
{-# INLINE iadj #-}

adjW ::
  (U.Unbox w) =>
  SparseGraph w ->
  Vertex ->
  U.Vector (Vertex, w)
adjW SparseGraph{..} v =
  U.zip
    (U.unsafeSlice o (o' - o) adjacentSG)
    (U.unsafeSlice o (o' - o) edgeCtxSG)
  where
    o = U.unsafeIndex offsetSG v
    o' = U.unsafeIndex offsetSG (v + 1)
{-# INLINE adjW #-}

iadjW ::
  (U.Unbox w) =>
  SparseGraph w ->
  Vertex ->
  U.Vector (EdgeId, Vertex, w)
iadjW SparseGraph{..} v =
  U.izipWith
    (\i u w -> (i + o, u, w))
    (U.unsafeSlice o (o' - o) adjacentSG)
    (U.unsafeSlice o (o' - o) edgeCtxSG)
  where
    o = U.unsafeIndex offsetSG v
    o' = U.unsafeIndex offsetSG (v + 1)
{-# INLINE iadjW #-}

outEdges :: SparseGraph w -> Vertex -> U.Vector EdgeId
outEdges SparseGraph{..} v = U.generate (o' - o) (+ o)
  where
    o = U.unsafeIndex offsetSG v
    o' = U.unsafeIndex offsetSG (v + 1)
{-# INLINE outEdges #-}

outDegree :: SparseGraph w -> Vertex -> Int
outDegree SparseGraph{..} v =
  U.unsafeIndex offsetSG (v + 1)
    - U.unsafeIndex offsetSG v
{-# INLINE outDegree #-}

outDegrees :: SparseGraph w -> U.Vector Int
outDegrees SparseGraph{..} = U.zipWith (-) (U.tail offsetSG) offsetSG
{-# INLINE outDegrees #-}
