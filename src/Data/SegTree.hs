module Data.SegTree where

import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import My.Prelude (ceilingPowerOf2, rep1, rev1)

{- | * @sendo@ is a monoid homomorphism (left monoid action)
   * @sendo f@ is a semigroup endomorphism
-}
class (Monoid f, Semigroup s) => AsSemigroupEndo f s where
  sendo :: f -> (s -> s)

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
    !n = ceilingPowerOf2 n0

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
    !n = ceilingPowerOf2 $ U.length xs

-- | /O(log n)/
readSegTree ::
  (AsSemigroupEndo f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
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
  (AsSemigroupEndo f a, Semigroup a, U.Unbox a, U.Unbox f, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  a ->
  m ()
writeSegTree st k0 v = do
  let !k = k0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st (k !>>. i)
  UM.unsafeWrite (getSegTree st) k v
  rep1 (heightSegTree st) $ \i -> do
    pullSegTree st (k !>>. i)
{-# INLINE writeSegTree #-}

-- | /O(log n)/
modifySegTree ::
  (AsSemigroupEndo f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  (a -> a) ->
  Int ->
  m ()
modifySegTree st f k0 = do
  let !k = k0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st (k !>>. i)
  UM.unsafeModify (getSegTree st) f k
  rep1 (heightSegTree st) $ \i -> do
    pullSegTree st (k !>>. i)
{-# INLINE modifySegTree #-}

{- | mappend [l..r)
 /O(log n)/
-}
mappendFromTo ::
  (AsSemigroupEndo f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  Int ->
  m a
mappendFromTo st l0 r0 = do
  let !l = l0 + sizeSegTree st
      !r = r0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    when ((l !>>. i) !<<. i /= l) $ do
      pushSegTree st (unsafeShiftR l i)
    when ((r !>>. i) !<<. i /= r) $ do
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
              ((l' + 1) !>>. 1)
              (r' !>>. 1)
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
  (AsSemigroupEndo f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
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
  (AsSemigroupEndo f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
appAt st k f = modifySegTree st (sendo f) k
{-# INLINE appAt #-}

{- | mapM_ (modify f) [l..r)
 /O(log n)/
-}
appFromTo ::
  (AsSemigroupEndo f a, Semigroup a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  Int ->
  f ->
  m ()
appFromTo st l0 r0 f = when (l0 < r0) $ do
  let !l = l0 + sizeSegTree st
      !r = r0 + sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    when ((l !>>. i) !<<. i /= l) $ do
      pushSegTree st (l !>>. i)
    when ((r !>>. i) !<<. i /= r) $ do
      pushSegTree st ((r - 1) !>>. i)

  fix
    ( \loop !l' !r' -> when (l' < r') $ do
        when (l' .&. 1 == 1) $ do
          evalAt st l' f
        when (r' .&. 1 == 1) $ do
          evalAt st (r' - 1) f
        loop
          ((l' + 1) !>>. 1)
          (r' !>>. 1)
    )
    l
    r

  rep1 (heightSegTree st) $ \i -> do
    when ((l !>>. i) !<<. i /= l) $ do
      pullSegTree st (l !>>. i)
    when ((r !>>. i) !<<. i /= r) $ do
      pullSegTree st ((r - 1) !>>. i)
{-# INLINE appFromTo #-}

-- | max r s.t. f (mappendFromTo seg l r) == True
upperBoundFrom ::
  (AsSemigroupEndo f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  -- | left
  Int ->
  -- | predicate s.t. f memepty == True, monotone
  (a -> Bool) ->
  m Int
upperBoundFrom st l p = do
  let !n = sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st ((l + n) !>>. i)
  violationNode <-
    fix
      ( \loopUp !acc !cur -> do
          let rightParent = cur !>>. countTrailingZeros cur
          !acc' <- (acc <>) <$> UM.unsafeRead (getSegTree st) rightParent
          if p acc'
            then do
              let !cur' = rightParent + 1
              if cur' .&. negate cur' /= cur'
                then loopUp acc' cur'
                else return Nothing
            else return $ Just (acc, rightParent)
      )
      mempty
      (l + n)
  case violationNode of
    Nothing -> return n
    Just (!acc0, !cur0) -> do
      fix
        ( \loopDown !acc !cur -> do
            if cur < n
              then do
                pushSegTree st cur
                let !leftChild = 2 * cur
                !acc' <- (acc <>) <$> UM.unsafeRead (getSegTree st) leftChild
                if p acc'
                  then loopDown acc' (leftChild + 1)
                  else loopDown acc leftChild
              else return $! cur - n
        )
        acc0
        cur0
{-# INLINE upperBoundFrom #-}

-- | min l s.t. f (mappendFromTo seg l r) == True
lowerBoundTo ::
  (AsSemigroupEndo f a, Monoid a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  -- | right
  Int ->
  -- | predicate s.t. f memepty == True, monotone
  (a -> Bool) ->
  m Int
lowerBoundTo st r p = do
  let !n = sizeSegTree st
  rev1 (heightSegTree st) $ \i -> do
    pushSegTree st ((r + n - 1) !>>. i)
  violationNode <-
    fix
      ( \loopUp !acc !cur -> do
          let leftParent =
                case cur !>>. countTrailingZeros (complement cur) of
                  0 -> 1 -- cur: 2 ^ n
                  v -> v
          !acc' <- (<> acc) <$> UM.unsafeRead (getSegTree st) leftParent
          if p acc'
            then do
              if leftParent .&. negate leftParent /= leftParent
                then loopUp acc' (leftParent - 1)
                else return Nothing
            else return $ Just (acc, leftParent)
      )
      mempty
      (r - 1 + n)
  case violationNode of
    Nothing -> return 0
    Just (!acc0, !cur0) ->
      fix
        ( \loopDown !acc !cur -> do
            if cur < n
              then do
                pushSegTree st cur
                let !rightChild = 2 * cur + 1
                !acc' <- (<> acc) <$> UM.unsafeRead (getSegTree st) rightChild
                if p acc'
                  then loopDown acc' (rightChild - 1)
                  else loopDown acc rightChild
              else return $! cur + 1 - n
        )
        acc0
        cur0
{-# INLINE lowerBoundTo #-}

-- | /O(1)/
evalAt ::
  (AsSemigroupEndo f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
  SegTree (PrimState m) f a ->
  Int ->
  f ->
  m ()
evalAt st k f = do
  UM.unsafeModify (getSegTree st) (sendo f) k
  when (k < sizeSegTree st) $ do
    UM.unsafeModify (getDualSegTree st) (f <>) k
{-# INLINE evalAt #-}

-- | /O(1)/
pushSegTree ::
  (AsSemigroupEndo f a, U.Unbox f, U.Unbox a, PrimMonad m) =>
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
  (<>)
    <$> UM.unsafeRead (getSegTree st) (2 * k)
    <*> UM.unsafeRead (getSegTree st) (2 * k + 1)
    >>= UM.unsafeWrite (getSegTree st) k
{-# INLINE pullSegTree #-}
