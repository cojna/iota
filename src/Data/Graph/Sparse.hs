{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Data.Graph.Sparse where

import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

type Vertex = Int
type Edge = (Vertex, Vertex)
type EdgeWith w = (Vertex, Vertex, w)
data SparseGraph w = CSR
    { numVerticesCSR :: !Int
    , numEdgesCSR    :: !Int
    , offsetCSR      :: !(U.Vector Int)
    , adjacentCSR    :: !(U.Vector Vertex)
    , edgeCtxCSR     :: !(U.Vector w)
    }

buildDirectedGraph
    :: Int -> U.Vector Edge -> SparseGraph ()
buildDirectedGraph numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . fst
            $ U.unzip edges
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

buildUndirectedGraph :: Int -> U.Vector Edge -> SparseGraph ()
buildUndirectedGraph numVerticesCSR edges = runST $ do
    let numEdgesCSR = 2 * U.length edges
    outDeg <- UM.replicate numVerticesCSR (0 :: Int)
    U.forM_ edges $ \(x, y) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
    offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(x, y) -> do
        posX <- UM.unsafeRead moffset x
        posY <- UM.unsafeRead moffset y
        UM.unsafeWrite moffset x (posX + 1)
        UM.unsafeWrite moffset y (posY + 1)
        UM.unsafeWrite madj posX y
        UM.unsafeWrite madj posY x
    adjacentCSR <- U.unsafeFreeze madj
    return CSR{edgeCtxCSR = U.replicate numEdgesCSR (), ..}

buildDirectedGraphW :: (U.Unbox w)
    => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildDirectedGraphW numVerticesCSR edges = runST $ do
    let numEdgesCSR = U.length edges
    let offsetCSR = U.scanl' (+) 0
            . U.unsafeAccumulate (+) (U.replicate numVerticesCSR 0)
            . U.map (flip (,) 1)
            . (\(x, _, _) -> x)
            $ U.unzip3 edges
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    mectx <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(src, dst, w) -> do
        pos <- UM.unsafeRead moffset src
        UM.unsafeWrite moffset src (pos + 1)
        UM.unsafeWrite madj pos dst
        UM.unsafeWrite mectx pos w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}

buildUndirectedGraphW :: (U.Unbox w)
    => Int -> U.Vector (EdgeWith w) -> SparseGraph w
buildUndirectedGraphW numVerticesCSR edges = runST $ do
    let numEdgesCSR = 2 * U.length edges
    outDeg <- UM.replicate numVerticesCSR (0 :: Int)
    U.forM_ edges $ \(x, y, _) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
    offsetCSR <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    moffset <- U.thaw offsetCSR
    madj <- UM.unsafeNew numEdgesCSR
    mectx <- UM.unsafeNew numEdgesCSR
    U.forM_ edges $ \(x, y, w) -> do
        posX <- UM.unsafeRead moffset x
        posY <- UM.unsafeRead moffset y
        UM.unsafeWrite moffset x (posX + 1)
        UM.unsafeWrite moffset y (posY + 1)
        UM.unsafeWrite madj posX y
        UM.unsafeWrite madj posY x
        UM.unsafeWrite mectx posX w
        UM.unsafeWrite mectx posY w
    adjacentCSR <- U.unsafeFreeze madj
    edgeCtxCSR <- U.unsafeFreeze mectx
    return CSR{..}

adj :: SparseGraph w -> Vertex -> U.Vector Vertex
adj CSR{..} v = U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adj #-}

iadj :: SparseGraph w -> Vertex -> U.Vector (Int, Vertex)
iadj CSR{..} v = U.imap ((,) . (+o)) $ U.unsafeSlice o (o' - o) adjacentCSR
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadj #-}

adjW :: (U.Unbox w)
    => SparseGraph w -> Vertex -> U.Vector (Vertex, w)
adjW CSR{..} v = U.zip
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE adjW #-}

iadjW :: (U.Unbox w)
    => SparseGraph w -> Vertex -> U.Vector (Int, Vertex, w)
iadjW CSR{..} v = U.izipWith (\i u w -> (i + o, u, w))
    (U.unsafeSlice o (o' - o) adjacentCSR)
    (U.unsafeSlice o (o' - o) edgeCtxCSR)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE iadjW #-}

outEdges :: SparseGraph w -> Vertex -> U.Vector Int
outEdges CSR{..} v = U.generate (o' - o) (+o)
  where
    o = U.unsafeIndex offsetCSR v
    o' = U.unsafeIndex offsetCSR (v + 1)
{-# INLINE outEdges #-}

