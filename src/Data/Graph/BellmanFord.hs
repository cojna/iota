module Data.Graph.BellmanFord where

import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude (rep)

type Vertex = Int

{- | Bellman-Ford O(VE)

 dist[v] == maxBound iff v is unreachable

 dist[v] == minBound iff v in negative cycle
-}
bellmanFord :: Int -> Vertex -> U.Vector (Vertex, Vertex, Int) -> U.Vector Int
bellmanFord n root edges = U.create $ do
  dist <- UM.replicate n maxBound
  UM.write dist root 0
  rep (n - 1) $ \_ -> do
    U.forM_ edges $ \(src, dst, cost) -> do
      dv <- UM.unsafeRead dist src
      du <- UM.unsafeRead dist dst
      when (dv + cost < du && dv /= maxBound) $ do
        UM.unsafeWrite dist dst $ dv + cost

  U.forM_ edges $ \(src, dst, cost) -> do
    dv <- UM.unsafeRead dist src
    du <- UM.unsafeRead dist dst
    when (dv + cost < du && dv /= maxBound) $ do
      UM.unsafeWrite dist dst minBound

  rep (n - 1) $ \_ -> do
    U.forM_ edges $ \(src, dst, _) -> do
      dv <- UM.unsafeRead dist src
      when (dv == minBound) $ do
        UM.unsafeWrite dist dst minBound

  return dist
