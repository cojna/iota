{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards #-}

module Data.Graph.Dense.Dijkstra where

import           Control.Monad               (when)
import qualified Data.Foldable               as F
import           Data.Function
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Utils                       (rep)

type Vertex = Int
type Cost = Int
data DenseGraph a = DenseGraph
    { numVerticesDG :: !Int
    , adjDG         :: U.Vector a
    }
instance (U.Unbox a, Show a) => Show (DenseGraph a) where
    show DenseGraph{..} =
        let n = numVerticesDG
        in F.foldMap (<> "\n")
            $ V.generate n $ \i ->
                show . U.toList $ U.unsafeSlice (i * n) n adjDG

-- | O(V^2)
dijkstraDense :: Vertex -> DenseGraph Cost -> U.Vector Cost
dijkstraDense src DenseGraph{..} = U.create $ do
    let n = numVerticesDG
    let ix i j = i * n + j
    dist <- UM.replicate (n + 1) maxBound
    used <- UM.replicate n False
    UM.write dist src 0
    let nothing = n
    vars <- UM.replicate 2 0
    let [_v, _dv] = [0..1]
    fix $ \loop -> do
        UM.unsafeWrite vars _v nothing
        UM.unsafeWrite vars _dv maxBound
        rep n $ \i -> do
            UM.unsafeRead used i >>= \case
                False -> do
                    di <- UM.unsafeRead dist i
                    dv <- UM.unsafeRead vars _dv
                    when (di < dv) $ do
                        UM.unsafeWrite vars _v i
                        UM.unsafeWrite vars _dv di
                True -> return ()
        v <- UM.unsafeRead vars _v
        when (v /= nothing) $ do
            UM.unsafeWrite used v True
            dv <- UM.unsafeRead vars _dv
            rep n $ \i -> do
                let di' = dv + U.unsafeIndex adjDG (ix v i)
                UM.unsafeModify dist (min di') i
            loop
    return dist
