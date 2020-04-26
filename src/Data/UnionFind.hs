{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.UnionFind where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype UnionFind s = UF { internalUF :: UM.MVector s Int }

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

freezeUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
freezeUnionFind = U.unsafeFreeze . internalUF

findUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findUF uf x = go x return
  where
    go !x k = do
        px <- UM.unsafeRead (internalUF uf) x
        if px < 0
        then k x
        else go px $ \ppx -> do
            UM.unsafeWrite (internalUF uf) x ppx
            k ppx
{-# INLINE findUF #-}

sizeUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeUF uf = fix $ \loop x -> do
    px <- UM.unsafeRead (internalUF uf) x
    if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeUF #-}

uniteUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
uniteUF uf x y = do
    px <- findUF uf x
    py <- findUF uf y
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
{-# INLINE uniteUF #-}

equivUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivUF uf x y = (==) `liftM` findUF uf x `ap` findUF uf y
{-# INLINE equivUF #-}

-- | O(n)
countGroupUF :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupUF uf = U.length . U.filter (<0) <$> freezeUnionFind uf
{-# INLINE countGroupUF #-}
