module Data.Graph.Dense.WarshallFloyd where

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           My.Prelude                  (rep)

-- | O(V^3)
warshallFloyd :: (PrimMonad m, U.Unbox a, Num a, Ord a)
    => Int -> UM.MVector (PrimState m) a -> m ()
warshallFloyd n d = do
    rep n $ \i -> do
        UM.unsafeWrite d (i * n + i) 0
    rep n $ \k -> do
        rep n $ \i -> do
            rep n $ \j -> do
                dij <- UM.unsafeRead d (i * n + j)
                dik <- UM.unsafeRead d (i * n + k)
                dkj <- UM.unsafeRead d (k * n + j)
                UM.unsafeWrite d (i * n + j) $ min dij (dik + dkj)
