{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards #-}

module Data.Graph.Dense.Dijkstra where

import           Control.Monad               (when)
import           Control.Monad.Primitive
import qualified Data.Foldable               as F
import           Data.Function
import           Data.Monoid
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Utils                       (rep)

-- | O(V^2)
dijkstraDense :: (U.Unbox w, Num w, Ord w, Bounded w)
    => Int -- ^ n
    -> Int -- ^ src
    -> U.Vector w -- ^ adjacent matrix (n x n)
    -> U.Vector w
dijkstraDense n src gr
    | src >= n || U.length gr /= n * n
        = error "dijkstraDense: Invalid Arguments"
    | otherwise = U.create $ do
        dist <- UM.replicate (n + 1) maxBound
        used <- UM.replicate n False
        UM.write dist src 0
        let nothing = n
        _v <- UM.replicate 1 0
        _dv <- UM.replicate 1 0
        fix $ \loop -> do
            UM.unsafeWrite _v 0 nothing
            UM.unsafeWrite _dv 0 maxBound
            rep n $ \i -> do
                UM.unsafeRead used i >>= \case
                    False -> do
                        di <- UM.unsafeRead dist i
                        dv <- UM.unsafeRead _dv 0
                        when (di < dv) $ do
                            UM.unsafeWrite _v 0 i
                            UM.unsafeWrite _dv 0 di
                    True -> return ()
            v <- UM.unsafeRead _v 0
            when (v /= nothing) $ do
                UM.unsafeWrite used v True
                dv <- UM.unsafeRead _dv 0
                rep n $ \i -> do
                    let di' = dv + U.unsafeIndex gr (v * n + i)
                    UM.unsafeModify dist (min di') i
                loop
        return dist

{-# INLINE dijkstraDense #-}
