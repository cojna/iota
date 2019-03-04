{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.UnionFind.Vector where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype UnionFind m = UF { parent :: UM.MVector m Int }

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

findM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findM uf x = go x return
  where
    go !x k = do
        px <- UM.unsafeRead (parent uf) x
        if px < 0
        then k x
        else go px $ \ppx -> do
            UM.unsafeWrite (parent uf) x ppx
            k ppx
{-# INLINE findM #-}

sizeM :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeM uf = fix $ \loop x -> do
    px <- UM.unsafeRead (parent uf) x
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
        rx <- UM.unsafeRead (parent uf) px
        ry <- UM.unsafeRead (parent uf) py
        if rx < ry
        then do
            UM.unsafeModify (parent uf) (+ry) px
            UM.unsafeWrite (parent uf) py px
        else do
            UM.unsafeModify (parent uf) (+rx) py
            UM.unsafeWrite (parent uf) px py
        return True
{-# INLINE uniteM #-}

equivM :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivM uf x y = (==) `liftM` findM uf x `ap` findM uf y
{-# INLINE equivM #-}

-- | O(n)
countGroupM :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupM uf = U.length . U.filter (<0) <$> U.unsafeFreeze (parent uf)
{-# INLINE countGroupM #-}
