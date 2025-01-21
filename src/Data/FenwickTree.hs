module Data.FenwickTree where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype FenwickTree s a = FenwickTree {getFenwickTree :: UM.MVector s a}

newFenwickTree ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  Int ->
  m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate (n + 1) mempty
{-# INLINE newFenwickTree #-}

-- | /O(n)/
buildFenwickTree ::
  (U.Unbox a, Monoid a, PrimMonad m) =>
  U.Vector a ->
  m (FenwickTree (PrimState m) a)
buildFenwickTree vec = do
  let n = U.length vec
  ft <- UM.unsafeNew (n + 1)
  UM.write ft 0 mempty
  U.unsafeCopy (UM.tail ft) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fti <- UM.unsafeRead ft i
      UM.unsafeModify ft (<> fti) j
    loop (i + 1)
  return $ FenwickTree ft
{-# INLINE buildFenwickTree #-}

{- | mappend [0..k)

 /O(log n)/
-}
mappendTo ::
  (PrimMonad m, U.Unbox a, Monoid a) =>
  FenwickTree (PrimState m) a ->
  Int ->
  m a
mappendTo (FenwickTree ft) = go mempty
  where
    go !acc !i
      | i > 0 = do
          xi <- UM.unsafeRead ft i
          go (acc <> xi) (i - (i .&. (-i)))
      | otherwise = return acc
{-# INLINE mappendTo #-}

-- | /O(log n)/
mappendAt ::
  (U.Unbox a, Semigroup a, PrimMonad m) =>
  FenwickTree (PrimState m) a ->
  Int ->
  a ->
  m ()
mappendAt (FenwickTree ft) k v = flip fix (k + 1) $ \loop !i -> do
  when (i < n) $ do
    UM.unsafeModify ft (<> v) i
    loop $ i + (i .&. (-i))
  where
    !n = UM.length ft
{-# INLINE mappendAt #-}
