{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Sparse where

import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.Tuple
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
--
import           Data.VecQueue

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = U.Vector Int

buildDirectedGraph :: Int -> U.Vector Edge -> Graph
buildDirectedGraph n edges = U.create $ do
    offset <- U.unsafeThaw
        . U.scanl' (+) (n + 1)
        . U.unsafeAccumulate (+) (U.replicate n 0)
        . U.map (flip (,) 1)
        . fst
        $ U.unzip edges
    gr <- UM.replicate (UM.length offset + U.length edges) 0
    U.forM_ (U.generate (n + 1) id) $ \i ->
        UM.unsafeRead offset i >>= UM.unsafeWrite gr i
    U.forM_ edges $ \(src, dst) -> do
        pos <- UM.unsafeRead offset src
        UM.unsafeWrite offset src (pos + 1)
        UM.unsafeWrite gr pos dst
    return gr

buildUndirectedGraph :: Int -> U.Vector Edge -> Graph
buildUndirectedGraph n edges
    = buildDirectedGraph n (edges U.++ U.map swap edges)

numVertices :: Graph -> Int
numVertices gr = U.head gr - 1

numEdges :: Graph -> Int
numEdges gr = U.length gr - U.head gr

vertices :: Graph -> U.Vector Vertex
vertices = flip U.generate id . numVertices

adjacent :: Graph -> Int -> U.Vector Vertex
adjacent gr v = U.unsafeSlice offset len gr
  where
    offset = U.unsafeIndex gr v
    len = U.unsafeIndex gr (v + 1) - offset

topSort :: Graph -> Maybe (U.Vector Int)
topSort gr = runST $ do
    let n = numVertices gr
    q <- withCapacityQ n
    let inDegree = U.unsafeAccumulate (+) (U.replicate n (0 :: Int))
            . U.map (flip (,) 1)
            $ U.drop (n + 1) gr
    U.mapM_ (flip enqueue q . fst)
        . U.filter ((== 0) . snd)
        $ U.indexed inDegree
    inDeg <- U.unsafeThaw inDegree
    fix $ \loop -> do
        dequeue q >>= \case
            Just v -> do
                U.forM_ (adjacent gr v) $ \u -> do
                    UM.unsafeRead inDeg u >>= \case
                        1 -> enqueue u q
                        i -> UM.unsafeWrite inDeg u (i - 1)
                loop
            Nothing -> return()
    res <- freezeQueueData q
    if U.length res == n
    then return $ Just res
    else return Nothing
