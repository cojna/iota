{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.SegTree where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import Data.Semigroup
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude (rep1, rev1, unsafeShiftRL)

{- | * @appMonoid mempty = id@
   * @appMonoid (f <> g) = appMonoid f . appMonoid g@
-}
class (Monoid f) => MonoidAction f a where
  appMonoid :: f -> a -> a

instance MonoidAction () m where
  appMonoid = const id
  {-# INLINE appMonoid #-}

instance MonoidAction (Min Int) (Min Int) where
  appMonoid = (<>)
  {-# INLINE appMonoid #-}

instance MonoidAction (Max Int) (Max Int) where
  appMonoid = (<>)
  {-# INLINE appMonoid #-}

{- | * @appMonoid f (x <> y) = appMonoid f x <> appMonoid f y@
   * @appMonoid (f <> g) = appMonoid f . appMonoid g@
   * @appMonoid mempty = id@
-}
data SegTree s f a = SegTree
  { getSegTree :: !(UM.MVector s a)
  , getDualSegTree :: !(UM.MVector s f)
  , sizeSegTree :: !Int
  , heightSegTree :: !Int
  }

newSegTree ::
  (Monoid f, U.Unbox f, Monoid a, U.Unbox a, PrimMonad m) =>
  Int ->
  m (SegTree (PrimState m) f a)
newSegTree n0 = do
  seg <- UM.replicate (2 * n) mempty
  dseg <- UM.replicate n mempty
  return $ SegTree seg dseg n (63 - countLeadingZeros n)
  where
    !n = extendToPowerOfTwo n0

-- | /O(n)/
buildSegTree ::
  (Monoid f, U.Unbox f, Monoid a, U.Unbox a, PrimMonad m) =>
  U.Vector a ->
  m (SegTree (PrimState m) f a)
buildSegTree xs = do
  seg <- UM.replicate (2 * n) mempty
  dseg <- UM.replicate n mempty
  U.unsafeCopy (UM.unsafeSlice n (U.length xs) seg) xs
  let st = SegTree seg dseg n (63 - countLeadingZeros n)
  rev1 (n - 1) $ \i -> do
    pullSegTree st i
  return st
  where
    !n = extendToPowerOfTwo $ U.length xs

-- | /O(log n)/
readSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  m a
readSegTree st k0 = do
  let !k = k0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st (unsafeShiftR k i)
  UM.unsafeRead (getSegTree st) k
{-# INLINE readSegTree #-}

-- | /O(log n)/
writeSegTree ::
  (MonoidAction f a, Semigroup a, U.Unbox a, U.Unbox f, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  a ->
  m ()
writeSegTree st k0 v = do
  let !k = k0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st (unsafeShiftR k i)
  UM.unsafeWrite (getSegTree st) k v
  rep1 (heightSegTree st) $ \i -> do
    pullSegTree st (unsafeShiftR k i)
{-# INLINE writeSegTree #-}

-- | /O(log n)/
modifySegTree ::
  (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  (a -> a) ->
  Int ->
  m ()
modifySegTree st f k0 = do
  let !k = k0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st (unsafeShiftR k i)
  UM.unsafeModify (getSegTree st) f k
  rep1 (heightSegTree st) $ \i -> do
    pullSegTree st (unsafeShiftR k i)
{-# INLINE modifySegTree #-}

{- | mappend [l..r)
 /O(log n)/
-}
mappendFromTo ::
  (MonoidAction f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  Int ->
  m a
mappendFromTo st l0 r0 = do
  let !l = l0 + sizeSegTree st
      !r = r0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
      pushSegTree st (unsafeShiftR l i)
    when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
      pushSegTree st (unsafeShiftR r i)

  fix
    ( \loop !accL !accR !l' !r' -> do
        if l' < r'
          then do
            !accL' <-
              if l' .&. 1 == 1
                then (accL <>) <$!> UM.unsafeRead (getSegTree st) l'
                else return accL
            !accR' <-
              if r' .&. 1 == 1
                then (<> accR) <$!> UM.unsafeRead (getSegTree st) (r' - 1)
                else return accR
            loop
              accL'
              accR'
              (unsafeShiftR (l' + l' .&. 1) 1)
              (unsafeShiftR (r' - r' .&. 1) 1)
          else return $! accL <> accR
    )
    mempty
    mempty
    l
    r
{-# INLINE mappendFromTo #-}

{- | mappend [0..k)
 /O(log n)/
-}
mappendTo ::
  (MonoidAction f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  m a
mappendTo st = mappendFromTo st 0
{-# INLINE mappendTo #-}

{- | mappend [0..n)
 /O(1)/
-}
mappendAll :: (U.Unbox a, PrimMonad m) => SegTree (PrimState m) f a -> m a
mappendAll st = UM.unsafeRead (getSegTree st) 1
{-# INLINE mappendAll #-}

{- | modify f k
 /O(log n)/
-}
appAt ::
  (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
appAt st k f = modifySegTree st (appMonoid f) k
{-# INLINE appAt #-}

{- | mapM_ (modify f) [l..r)
 /O(log n)/
-}
appFromTo ::
  (MonoidAction f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  Int ->
  f ->
  m ()
appFromTo st l0 r0 f = when (l0 < r0) $ do
  let !l = l0 + sizeSegTree st
      !r = r0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
      pushSegTree st (unsafeShiftRL l i)
    when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
      pushSegTree st (unsafeShiftRL (r - 1) i)

  fix
    ( \loop !l' !r' -> when (l' < r') $ do
        when (l' .&. 1 == 1) $ do
          evalAt st l' f
        when (r' .&. 1 == 1) $ do
          evalAt st (r' - 1) f
        loop
          (unsafeShiftR (l' + l' .&. 1) 1)
          (unsafeShiftR (r' - r' .&. 1) 1)
    )
    l
    r

  rep1 (heightSegTree st) $ \i -> do
    when (unsafeShiftR l i `unsafeShiftL` i /= l) $ do
      pullSegTree st (unsafeShiftRL l i)
    when (unsafeShiftR r i `unsafeShiftL` i /= r) $ do
      pullSegTree st (unsafeShiftRL (r - 1) i)
{-# INLINE appFromTo #-}

-- | /O(1)/
evalAt ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
evalAt st k f = do
  UM.unsafeModify (getSegTree st) (appMonoid f) k
  when (k < sizeSegTree st) $ do
    UM.unsafeModify (getDualSegTree st) (f <>) k
{-# INLINE evalAt #-}

-- | /O(1)/
pushSegTree ::
  (MonoidAction f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  m ()
pushSegTree st k = do
  fk <- UM.unsafeRead (getDualSegTree st) k
  UM.unsafeWrite (getDualSegTree st) k mempty
  evalAt st (2 * k) fk
  evalAt st (2 * k + 1) fk
{-# INLINE pushSegTree #-}

-- | /O(1)/
pullSegTree ::
  (Semigroup a, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  m ()
pullSegTree st k = do
  (<>) <$> UM.unsafeRead (getSegTree st) (2 * k)
    <*> UM.unsafeRead (getSegTree st) (2 * k + 1)
    >>= UM.unsafeWrite (getSegTree st) k
{-# INLINE pullSegTree #-}

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
  | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
  | otherwise = 1
