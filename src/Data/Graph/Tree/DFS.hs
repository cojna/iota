{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Tree.DFS where

import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.VecStack
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

type VertexId = Int
type EdgeId = Int
type Vertex = ()
type Edge = (Int, Int)
type WEdge = (Int, Int, Int)

data Graph v e = Graph
    { offset :: !(U.Vector Int)
    , vertices :: !(U.Vector v)
    , edges :: !(U.Vector e)
    }

adj :: (U.Unbox v, U.Unbox e) => Graph v e -> VertexId -> U.Vector e
adj Graph{..} v = U.unsafeSlice o (o' - o) edges
  where
    o  = U.unsafeIndex offset v
    o' = U.unsafeIndex offset (v + 1)
{-# INLINE adj #-}

iadj :: (U.Unbox v, U.Unbox e) => Graph v e -> VertexId -> U.Vector (Int, e)
iadj Graph{..} v = U.imap (\i e -> (o + i, e))
    $ U.unsafeSlice o (o' - o) edges
  where
    o  = U.unsafeIndex offset v
    o' = U.unsafeIndex offset (v + 1)
{-# INLINE iadj #-}

undirectedWGraph
    :: (U.Unbox v)
    => U.Vector v -> U.Vector (Int, Int, Int) -> Graph v WEdge
undirectedWGraph vertices es = runST $ do
    mbuf <- UM.unsafeNew numE
    outDeg <- UM.replicate numV 0
    flip U.imapM_ es $ \i (x, y, d) -> do
        UM.unsafeModify outDeg (+1) x
        UM.unsafeModify outDeg (+1) y
        UM.unsafeWrite mbuf (2 * i) (x, y, d)
        UM.unsafeWrite mbuf (2 * i + 1) (y, x, d)
    offset <- U.scanl' (+) 0 <$> U.unsafeFreeze outDeg
    buf <- U.unsafeFreeze mbuf

    mpos <- U.thaw offset
    medges <- UM.unsafeNew numE
    U.forM_ buf $ \e@(src, _, _) -> do
        p <- UM.unsafeRead mpos src
        UM.unsafeWrite medges p e
        UM.unsafeModify mpos (+1) src
    edges <- U.unsafeFreeze medges
    _ <- U.unsafeFreeze mpos
    return $! Graph{..}
  where
    numV = U.length vertices
    numE = 2 * U.length es

shortestPath :: (U.Unbox v) => Graph v WEdge -> VertexId -> U.Vector Int
shortestPath gr@Graph{..} root = U.create $ do
    dist <- UM.unsafeNew (U.length vertices)
    UM.unsafeWrite dist root 0
    stack <- newStackM
    U.forM_ (iadj gr root) $ \(i, _) ->
        pushM i stack

    fix $ \loop ->
        popM stack >>= \case
            Just ei -> do
                let (p, v, d) = U.unsafeIndex edges ei
                UM.unsafeRead dist p >>= UM.unsafeWrite dist v . (+d)
                let nexts = U.map fst
                        . U.filter (\(_, (_, dst, _)) -> dst /= p)
                        $ iadj gr v
                U.forM_ nexts $ \i ->
                    pushM i stack
                loop
            Nothing -> return ()
    return dist

diameter :: (U.Unbox v) => Graph v WEdge -> Int
diameter tree = U.maximum
    . shortestPath tree
    . U.maxIndex
    $ shortestPath tree 0

height :: (U.Unbox v) => Graph v WEdge -> U.Vector Int
height tree = U.zipWith max fromS fromT
  where
    s = U.maxIndex $ shortestPath tree 0
    fromS = shortestPath tree s
    t = U.maxIndex fromS
    fromT = shortestPath tree t
