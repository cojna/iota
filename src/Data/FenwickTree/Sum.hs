module Data.FenwickTree.Sum where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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

/O(lof n)/
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
readSumFenwickTree ::
  (Num a, U.Unbox a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  m a
readSumFenwickTree ft i = sumFromTo ft i (i + 1)
{-# INLINE readSumFenwickTree #-}

-- | /O(log n)/
writeSumFenwickTree ::
  (Num a, U.Unbox a, PrimMonad m) =>
  SumFenwickTree (PrimState m) a ->
  Int ->
  a ->
  m ()
writeSumFenwickTree ft i x = readSumFenwickTree ft i >>= addAt ft i . (x -)
{-# INLINE writeSumFenwickTree #-}
