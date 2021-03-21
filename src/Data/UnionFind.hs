{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Data.UnionFind where

import Control.Monad
import Control.Monad.Primitive
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype UnionFind s = UF {getUnionFind :: UM.MVector s Int}

newUnionFind :: PrimMonad m => Int -> m (UnionFind (PrimState m))
newUnionFind n = UF <$> UM.replicate n (-1)
{-# INLINE newUnionFind #-}

freezeUnionFind :: PrimMonad m => UnionFind (PrimState m) -> m (U.Vector Int)
freezeUnionFind = U.freeze . getUnionFind

findUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
findUF uf x = go x return
  where
    go !x k = do
      px <- UM.unsafeRead (getUnionFind uf) x
      if px < 0
        then k x
        else go px $ \ppx -> do
          UM.unsafeWrite (getUnionFind uf) x ppx
          k ppx
{-# INLINE findUF #-}

sizeUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
sizeUF uf = fix $ \loop x -> do
  px <- UM.unsafeRead (getUnionFind uf) x
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
      rx <- UM.unsafeRead (getUnionFind uf) px
      ry <- UM.unsafeRead (getUnionFind uf) py
      if rx < ry
        then do
          UM.unsafeModify (getUnionFind uf) (+ ry) px
          UM.unsafeWrite (getUnionFind uf) py px
        else do
          UM.unsafeModify (getUnionFind uf) (+ rx) py
          UM.unsafeWrite (getUnionFind uf) px py
      return True
{-# INLINE uniteUF #-}

uniteUF_ :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m ()
uniteUF_ uf x y = void $ uniteUF uf x y
{-# INLINE uniteUF_ #-}

equivUF :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m Bool
equivUF uf x y = (==) `liftM` findUF uf x `ap` findUF uf y
{-# INLINE equivUF #-}

-- | O(n)
countGroupUF :: PrimMonad m => UnionFind (PrimState m) -> m Int
countGroupUF uf = U.length . U.filter (< 0) <$> freezeUnionFind uf
{-# INLINE countGroupUF #-}
