{-# LANGUAGE BangPatterns, LambdaCase, RecordWildCards #-}

module Data.Graph.Dense.Prim where

import           Control.Monad                     (when)
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as UM

import           My.Prelude                        (rep, stream)

-- | /O(V^2)/
primDense :: (U.Unbox w, Num w, Ord w)
    => Int -- ^ n
    -> Int -- ^ root
    -> U.Vector w -- ^ adjacent matrix (n x n)
    -> U.Vector Int -- ^ parent (parent[root] = -1)
primDense n root gr
    | root >= n || U.length gr /= n * n
        = error "primDense: Invalid Arguments"
    | otherwise = U.create $ do
        let !inf = 2 * U.maximum gr
        parent <- UM.replicate n (-1)
        dist <- UM.new n
        used <- UM.replicate n False
        UM.write dist root 0
        UM.write used root True
        rep n $ \i -> do
            when (i /= root) $ do
                UM.write parent i root
            UM.write dist i $ U.unsafeIndex gr (root * n + i)
        rep (n - 1) $ \_ -> do
            v <- fmap snd $ MS.foldM'
                (\acc i -> do
                    UM.unsafeRead used i >>= \case
                        False -> do
                            d <- UM.unsafeRead dist i
                            return $! min acc (d, i)
                        True -> return acc
                ) (inf, (-1)) $ stream 0 n
            UM.write used v True
            rep n $ \u -> do
                UM.unsafeRead used u >>= \case
                    False -> do
                        du <- UM.unsafeRead dist u
                        let dvu = U.unsafeIndex gr (v * n + u)
                        when (dvu < du) $ do
                            UM.unsafeWrite dist u dvu
                            UM.unsafeWrite parent u v
                    True -> return ()
        return parent
{-# INLINE primDense #-}
