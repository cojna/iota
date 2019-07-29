{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.UnionFind.Vector where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype UnionFind s = UF { internalUF :: UM.MVector s Int }

runUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
runUnionFind = U.unsafeFreeze . internalUF

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf x = go x return
  where
    go !x k = do
        px <- UM.unsafeRead (internalUF uf) x
        if px < 0
        then k x
        else go px $ \ppx -> do
            UM.unsafeWrite (internalUF uf) x ppx
            k ppx
{-# INLINE findM #-}

sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf = fix $ \loop x -> do
    px <- UM.unsafeRead (internalUF uf) x
    if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeM #-}

uniteM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteM uf x y = do
    px <- findM uf x
    py <- findM uf y
    if px == py
    then return False
    else do
        rx <- UM.unsafeRead (internalUF uf) px
        ry <- UM.unsafeRead (internalUF uf) py
        if rx < ry
        then do
            UM.unsafeModify (internalUF uf) (+ry) px
            UM.unsafeWrite  (internalUF uf) py px
        else do
            UM.unsafeModify (internalUF uf) (+rx) py
            UM.unsafeWrite  (internalUF uf) px py
        return True
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM uf = U.length . U.filter (<0) <$> runUnionFind uf
{-# INLINE countGroupM #-}
