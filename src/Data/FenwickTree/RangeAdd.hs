module Data.FenwickTree.RangeAdd where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude (floorPowerOf2, rep1, rev1)

newtype RangeAddFenwickTree s a = RangeAddFenwickTree (UM.MVector s a)

newRangeAddFenwickTree ::
  (Num a, U.Unbox a, PrimMonad m) =>
  Int ->
  m (RangeAddFenwickTree (PrimState m) a)
newRangeAddFenwickTree n = RangeAddFenwickTree <$> UM.replicate (n + 1) 0
{-# INLINE newRangeAddFenwickTree #-}

buildRangeAddFenwickTree ::
  forall a m.
  (Num a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (RangeAddFenwickTree (PrimState m) a)
buildRangeAddFenwickTree xs = do
  let n = U.length xs
  ft <- UM.unsafeNew (n + 1)
  UM.write ft 0 0
  U.unsafeCopy (UM.tail ft) xs
  rev1 n $ \i -> do
    x <- UM.unsafeRead ft (i - 1)
    UM.unsafeModify ft (subtract x) i
  rep1 n $ \i -> do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fti <- UM.unsafeRead ft i
      UM.unsafeModify ft (+ fti) j
  return $ RangeAddFenwickTree ft
{-# INLINE buildRangeAddFenwickTree #-}

addFromTo ::
  (Num a, U.Unbox a, PrimMonad m) =>
  RangeAddFenwickTree (PrimState m) a ->
  Int ->
  Int ->
  a ->
  m ()
addFromTo (RangeAddFenwickTree ft) l r x = do
  add l x
  add r (-x)
  where
    !n = UM.length ft
    add k v = flip fix (k + 1) $ \loop !i -> do
      when (i < n) $ do
        UM.unsafeModify ft (+ v) i
        loop $ i + (i .&. (-i))
{-# INLINE addFromTo #-}

-- | /O(log n)/
readRAFT ::
  (Num a, U.Unbox a, PrimMonad m) =>
  RangeAddFenwickTree (PrimState m) a ->
  Int ->
  m a
readRAFT (RangeAddFenwickTree ft) k = go 0 (k + 1)
  where
    go !acc !i
      | i > 0 = do
          xi <- UM.unsafeRead ft i
          go (acc + xi) (i - (i .&. (-i)))
      | otherwise = return acc
{-# INLINE readRAFT #-}

-- | /O(log n)/
writeRAFT ::
  (Num a, U.Unbox a, PrimMonad m) =>
  RangeAddFenwickTree (PrimState m) a ->
  Int ->
  a ->
  m ()
writeRAFT ft i x = do
  fi <- readRAFT ft i
  addFromTo ft i (i + 1) (x - fi)
{-# INLINE writeRAFT #-}

{- |
>>> ft <- buildRangeAddFenwickTree @Int (U.fromList [1,2,2,3,3,3])
>>> lowerBoundRAFT ft 2
1
>>> lowerBoundRAFT ft 0
0
>>> lowerBoundRAFT ft 10
6
-}
lowerBoundRAFT ::
  (U.Unbox a, Num a, Ord a, PrimMonad m) =>
  RangeAddFenwickTree (PrimState m) a ->
  a ->
  m Int
lowerBoundRAFT (RangeAddFenwickTree ft) s0
  | s0 <= 0 = return 0
  | otherwise = go s0 (floorPowerOf2 n) 0
  where
    !n = UM.length ft
    go !s !w !i
      | w == 0 = return i
      | otherwise = do
          if i + w < n
            then do
              fiw <- UM.unsafeRead ft (i + w)
              if fiw < s
                then go (s - fiw) (w !>>. 1) (i + w)
                else go s (w !>>. 1) i
            else go s (w !>>. 1) i
{-# INLINE lowerBoundRAFT #-}

{- |
>>> ft <- buildRangeAddFenwickTree @Int (U.fromList [1,2,2,3,3,3])
>>> upperBoundRAFT ft 2
3
>>> upperBoundRAFT ft 0
0
>>> upperBoundRAFT ft 10
6
-}
upperBoundRAFT ::
  (U.Unbox a, Num a, Ord a, PrimMonad m) =>
  RangeAddFenwickTree (PrimState m) a ->
  a ->
  m Int
upperBoundRAFT (RangeAddFenwickTree ft) s0
  | s0 < 0 = return 0
  | otherwise = go s0 (floorPowerOf2 n) 0
  where
    !n = UM.length ft
    go !s !w !i
      | w == 0 = return i
      | otherwise = do
          if i + w < n
            then do
              fiw <- UM.unsafeRead ft (i + w)
              if fiw <= s
                then go (s - fiw) (w !>>. 1) (i + w)
                else go s (w !>>. 1) i
            else go s (w !>>. 1) i
{-# INLINE upperBoundRAFT #-}
