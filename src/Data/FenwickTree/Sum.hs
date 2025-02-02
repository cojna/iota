module Data.FenwickTree.Sum where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude (floorPowerOf2)

newtype SumFenwickTree s a = SumFenwickTree {getSumFenwickTree :: UM.MVector s a}

newSumFenwickTree ::
  (U.Unbox a, Num a, PrimMonad m) =>
  Int ->
  m (SumFenwickTree (PrimState m) a)
newSumFenwickTree n = SumFenwickTree <$> UM.replicate (n + 1) 0
{-# INLINE newSumFenwickTree #-}

-- | /O(n)/
buildSumFenwickTree ::
  (U.Unbox a, Num a, PrimMonad m) =>
  U.Vector a ->
  m (SumFenwickTree (PrimState m) a)
buildSumFenwickTree vec = do
  let n = U.length vec
  ft <- UM.unsafeNew (n + 1)
  UM.write ft 0 0
  U.unsafeCopy (UM.tail ft) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fti <- UM.unsafeRead ft i
      UM.unsafeModify ft (+ fti) j
    loop (i + 1)
  return $ SumFenwickTree ft
{-# INLINE buildSumFenwickTree #-}

{- | sum[0..k)

 /O(log n)/
-}
sumTo ::
  (PrimMonad m, U.Unbox a, Num a) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  m a
sumTo (SumFenwickTree ft) = go 0
  where
    go !acc !i
      | i > 0 = do
          xi <- UM.unsafeRead ft i
          go (acc + xi) (i - (i .&. (-i)))
      | otherwise = return acc
{-# INLINE sumTo #-}

{- | sum[l..r)

/O(log n)/
-}
sumFromTo ::
  (PrimMonad m, U.Unbox a, Num a) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  Int ->
  m a
sumFromTo (SumFenwickTree ft) = goL 0
  where
    goL !acc !l !r
      | l > 0 = do
          xl' <- (acc -) <$> UM.unsafeRead ft l
          goL xl' (l - (l .&. (-l))) r
      | otherwise = goR acc r
    goR !acc !r
      | r > 0 = do
          xr' <- (acc +) <$> UM.unsafeRead ft r
          goR xr' (r - (r .&. (-r)))
      | otherwise = return acc
{-# INLINE sumFromTo #-}

-- | /O(log n)/
addAt ::
  (U.Unbox a, Num a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  a ->
  m ()
addAt (SumFenwickTree ft) k v = flip fix (k + 1) $ \loop !i -> do
  when (i < n) $ do
    UM.unsafeModify ft (+ v) i
    loop $ i + (i .&. (-i))
  where
    !n = UM.length ft
{-# INLINE addAt #-}

-- | /O(log n)/
readSFT ::
  (Num a, U.Unbox a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  m a
readSFT ft i = sumFromTo ft i (i + 1)
{-# INLINE readSFT #-}

-- | /O(log n)/
writeSFT ::
  (Num a, U.Unbox a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  a ->
  m ()
writeSFT ft i x = readSFT ft i >>= addAt ft i . (x -)
{-# INLINE writeSFT #-}

{- |
min i s.t. sum [0..i) >= w

>>> ft <- buildSumFenwickTree @Int (U.fromList [1,1,1,1,1])
>>> lowerBoundSFT ft 3
3
>>> sumTo ft 3
3
>>> lowerBoundSFT ft 0
0
>>> lowerBoundSFT ft 1
1
>>> lowerBoundSFT ft 10
6

>>> ft <- buildSumFenwickTree @Int (U.fromList [1,1,0,0,0])
>>> lowerBoundSFT ft 2
2
-}
lowerBoundSFT ::
  (U.Unbox a, Num a, Ord a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  a ->
  m Int
lowerBoundSFT (SumFenwickTree ft) s0
  | s0 <= 0 = return 0
  | otherwise = go s0 (floorPowerOf2 n) 0
  where
    !n = UM.length ft
    go !s !w !i
      | w == 0 = return $! i + 1
      | otherwise = do
          if i + w < n
            then do
              fiw <- UM.unsafeRead ft (i + w)
              if fiw < s
                then go (s - fiw) (w !>>. 1) (i + w)
                else go s (w !>>. 1) i
            else go s (w !>>. 1) i
{-# INLINE lowerBoundSFT #-}

{- |
max i s.t. sum [0..i) <= w

>>> ft <- buildSumFenwickTree @Int (U.fromList [1,1,1,1,1])
>>> upperBoundSFT ft 3
3
>>> sumTo ft 3
3
>>> upperBoundSFT ft 0
0
>>> upperBoundSFT ft 1
1
>>> upperBoundSFT ft 10
5

>>> ft <- buildSumFenwickTree @Int (U.fromList [1,1,0,0,1])
>>> upperBoundSFT ft 2
4
-}
upperBoundSFT ::
  (U.Unbox a, Num a, Ord a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  a ->
  m Int
upperBoundSFT (SumFenwickTree ft) s0
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
{-# INLINE upperBoundSFT #-}
