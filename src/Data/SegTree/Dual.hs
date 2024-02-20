{-# LANGUAGE RecordWildCards #-}

module Data.SegTree.Dual where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import My.Prelude

import Data.Monoid.Action (MonoidAction (..))

-- | @a@ need not to be Semigroup
data DualSegTree s f a = DualSegTree
  { dualSegTreeDST :: !(UM.MVector s f)
  , primalsDST :: !(UM.MVector s a)
  , dualSizeDST :: !Int
  -- ^ @2^n@
  , primalSizeDST :: !Int
  , heightDST :: !Int
  -- ^ @2 ^ height == sizeDual@
  }

-- | /O(n)/
buildDualSegTree ::
  (Monoid f, U.Unbox f, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (DualSegTree (PrimState m) f a)
buildDualSegTree xs = do
  dualSegTreeDST <- UM.replicate dualSizeDST mempty
  primalsDST <- U.thaw xs
  return $ DualSegTree{..}
  where
    !dualSizeDST = extendToPowerOfTwo $ U.length xs
    primalSizeDST = U.length xs
    heightDST = countTrailingZeros dualSizeDST
{-# INLINE buildDualSegTree #-}

-- | /O(n)/
freezeDualSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  m (U.Vector a)
freezeDualSegTree
  DualSegTree
    { dualSegTreeDST = dseg
    , primalsDST = pseg
    , dualSizeDST = dsize
    } = do
    rep1 ((dsize - 1) !>>. 1) $ \i -> do
      fi <- UM.unsafeRead dseg i
      UM.unsafeModify dseg (fi <>) (2 * i)
      UM.unsafeModify dseg (fi <>) (2 * i + 1)

    flip UM.imapM_ pseg $ \i x -> do
      f <- UM.unsafeRead dseg $ (i + dsize) !>>. 1
      UM.unsafeWrite pseg i (mact f x)

    UM.set dseg mempty
    U.freeze pseg
{-# INLINE freezeDualSegTree #-}

-- | /O(log n)/
readDualSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  m a
readDualSegTree
  seg@DualSegTree
    { primalsDST = pseg
    , dualSizeDST = dsize
    , heightDST = height
    }
  k0 = do
    let !k = k0 + dsize
    rev1 height $ \i -> do
      pushSegTree seg (k !>>. i)
    UM.unsafeRead pseg k0
{-# INLINE readDualSegTree #-}

-- | /O(log n)/
writeDualSegTree ::
  (MonoidAction f a, U.Unbox a, U.Unbox f, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  a ->
  m ()
writeDualSegTree
  seg@DualSegTree
    { primalsDST = pseg
    , dualSizeDST = dsize
    , heightDST = height
    }
  k0
  v = do
    let !k = k0 + dsize
    rev1 height $ \i -> do
      pushSegTree seg (k !>>. i)
    UM.unsafeWrite pseg k0 v
{-# INLINE writeDualSegTree #-}

-- | /O(log n)/
modifyDualSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  (a -> a) ->
  Int ->
  m ()
modifyDualSegTree
  seg@DualSegTree
    { primalsDST = pseg
    , dualSizeDST = dsize
    , heightDST = height
    }
  f
  k0 = do
    let !k = k0 + dsize
    rev1 height $ \i -> do
      pushSegTree seg (k !>>. i)
    UM.unsafeModify pseg f k0
{-# INLINE modifyDualSegTree #-}

{- | modify f k
 /O(log n)/
-}
appAt ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
appAt st k f = modifyDualSegTree st (mact f) k
{-# INLINE appAt #-}

{- | mapM_ (modify f) [l..r)
 /O(log n)/
-}
appFromTo ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  Int ->
  f ->
  m ()
appFromTo
  seg@DualSegTree
    { dualSizeDST = dsize
    , heightDST = height
    }
  l0
  r0
  f = when (l0 < r0) $ do
    let !l = l0 + dsize
        !r = r0 + dsize
    rev1 height $ \i -> do
      when ((l !>>. i) !<<. i /= l) $ do
        pushSegTree seg (l !>>. i)
      when ((r !>>. i) !<<. i /= r) $ do
        pushSegTree seg ((r - 1) !>>. i)

    fix
      ( \loop !l' !r' -> when (l' < r') $ do
          when (l' .&. 1 == 1) $ do
            evalAt seg l' f
          when (r' .&. 1 == 1) $ do
            evalAt seg (r' - 1) f
          loop ((l' + 1) !>>. 1) (r' !>>. 1)
      )
      l
      r
{-# INLINE appFromTo #-}

-- | /O(1)/
evalAt ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
evalAt
  DualSegTree
    { dualSegTreeDST = dseg
    , primalsDST = pseg
    , dualSizeDST = dsize
    , primalSizeDST = psize
    }
  k
  f = do
    if k < dsize
      then UM.unsafeModify dseg (f <>) k
      else do
        let !k' = k - dsize
        when (k' < psize) $ do
          UM.unsafeModify pseg (mact f) k'
{-# INLINE evalAt #-}

-- | /O(1)/
pushSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  DualSegTree (PrimState m) f a ->
  Int ->
  m ()
pushSegTree seg@DualSegTree{dualSegTreeDST = dseg} k = do
  fk <- UM.unsafeRead dseg k
  UM.unsafeWrite dseg k mempty
  evalAt seg (2 * k) fk
  evalAt seg (2 * k + 1) fk
{-# INLINE pushSegTree #-}

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
  | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
  | otherwise = 1
