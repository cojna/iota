module Data.UnionFind.Diff where

import Control.Monad.Primitive
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data UnionFindDiff s a = UFD
  { parentOrNegativeSizeUFD :: UM.MVector s Int
  , potentialUFD :: UM.MVector s a
  }

newUnionFindDiff ::
  (U.Unbox a, Num a, PrimMonad m) =>
  Int ->
  m (UnionFindDiff (PrimState m) a)
newUnionFindDiff n =
  UFD
    <$> UM.replicate n (-1)
    <*> UM.replicate n 0

findUFD ::
  (Num a, U.Unbox a, PrimMonad m) =>
  UnionFindDiff (PrimState m) a ->
  Int ->
  -- | (representative, potential)
  m (Int, a)
findUFD (UFD uf potential) x0 = go x0 return
  where
    go !x k = do
      px <- UM.unsafeRead uf x
      if px < 0
        then k (x, 0)
        else go px $ \(root, !hpx) -> do
          UM.unsafeWrite uf x root
          hx <- UM.unsafeRead potential x
          let !hx' = hpx + hx
          UM.unsafeWrite potential x hx'
          k (root, hx')
{-# INLINE findUFD #-}

sizeUFD :: (PrimMonad m) => UnionFindDiff (PrimState m) a -> Int -> m Int
sizeUFD (UFD uf _) = fix $ \loop x -> do
  px <- UM.unsafeRead uf x
  if px < 0
    then return $! negate px
    else loop px
{-# INLINE sizeUFD #-}

{- | @hx - hy = d@

>>> uf <- newUnionFindDiff @Int 2
>>> setDiffUFD uf 1 0 1
Just True
>>> setDiffUFD uf 1 0 999
Nothing
>>> setDiffUFD uf 1 0 1
Just False
>>> setDiffUFD uf 0 1 (-1)
Just False
-}
setDiffUFD ::
  (Eq a, Num a, U.Unbox a, PrimMonad m) =>
  UnionFindDiff (PrimState m) a ->
  Int ->
  Int ->
  a ->
  m (Maybe Bool)
setDiffUFD ufd@(UFD uf potential) x y d = do
  (px, hx) <- findUFD ufd x
  (py, hy) <- findUFD ufd y
  if px == py
    then
      if hx - hy == d
        then return $ Just False
        else return Nothing
    else do
      rx <- UM.unsafeRead uf px
      ry <- UM.unsafeRead uf py
      if rx < ry
        then do
          UM.unsafeModify uf (+ ry) px
          UM.unsafeWrite uf py px
          UM.unsafeWrite potential py $ hx - hy - d
        else do
          UM.unsafeModify uf (+ rx) py
          UM.unsafeWrite uf px py
          UM.unsafeWrite potential px $ hy - hx + d
      return $ Just True
{-# INLINE setDiffUFD #-}

{- | @hx - hy@

>>> uf <- newUnionFindDiff @Int 3
>>> setDiffUFD uf 1 0 1
Just True
>>> diffUFD uf 1 0
Just 1
>>> diffUFD uf 0 1
Just (-1)
>>> diffUFD uf 0 2
Nothing
>>> setDiffUFD uf 2 1 2
Just True
>>> diffUFD uf 2 1
Just 2
>>> diffUFD uf 2 0
Just 3
-}
diffUFD ::
  (Num a, U.Unbox a, PrimMonad m) =>
  UnionFindDiff (PrimState m) a ->
  Int ->
  Int ->
  m (Maybe a)
diffUFD ufd x y = do
  (px, hx) <- findUFD ufd x
  (py, hy) <- findUFD ufd y
  pure
    $ if px == py
      then Just $! hx - hy
      else Nothing
{-# INLINE diffUFD #-}
