{-# LANGUAGE BangPatterns #-}

module Data.FenwickTree where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word

newtype FenwickTree s a = FenwickTree {getFenwickTree :: UM.MVector s a}

newFenwickTree :: (PrimMonad m, U.Unbox a, Monoid a)
    => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate (n + 1) mempty
{-# INLINE newFenwickTree #-}

-- | /O(n)/
buildFenwickTree :: (PrimMonad m, U.Unbox a, Monoid a)
    => U.Vector a -> m (FenwickTree (PrimState m) a)
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

-- | mappend [0..k)
--
-- /O(log n)/
mappendTo :: (PrimMonad m, U.Unbox a, Monoid a)
    => FenwickTree (PrimState m) a -> Int -> m a
mappendTo (FenwickTree ft) = go mempty
  where
    go !acc !i
        | i > 0 = do
            xi <- UM.unsafeRead ft i
            go (acc <> xi) (i - (i .&. (-i)))
        | otherwise = return acc
{-# INLINE mappendTo #-}

-- | sum [0..k)
--
-- /O(log n)/
sumTo :: (PrimMonad m, U.Unbox a, Num a)
    => FenwickTree (PrimState m) (Sum a) -> Int ->  m (Sum a)
sumTo = mappendTo
{-# INLINE sumTo #-}

-- | sum [l..r)
--
-- /O(log n)/
sumFromTo :: (PrimMonad m, U.Unbox a, Num a)
    => FenwickTree (PrimState m) (Sum a) -> Int -> Int -> m (Sum a)
sumFromTo ft l r = (-) <$> sumTo ft r <*> sumTo ft l
{-# INLINE sumFromTo #-}

-- /O(log n)/
readFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => FenwickTree (PrimState m) (Sum a) -> Int -> m (Sum a)
readFenwickTree ft i = sumFromTo ft i (i + 1)
{-# INLINE readFenwickTree #-}

-- /O(log n)/
writeFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => FenwickTree (PrimState m) (Sum a) -> Int -> (Sum a) -> m ()
writeFenwickTree ft i x = readFenwickTree ft i >>= mappendAt ft i . (x-)
{-# INLINE writeFenwickTree #-}

-- | /O(log n)/
mappendAt :: (PrimMonad m, U.Unbox a, Semigroup a)
    => FenwickTree (PrimState m) a -> Int -> a -> m ()
mappendAt (FenwickTree ft) k v = flip fix (k + 1) $ \loop !i -> do
    when (i < n) $ do
        UM.unsafeModify ft (<> v) i
        loop $ i + (i .&. (-i))
  where
    !n = UM.length ft
{-# INLINE mappendAt #-}

-- | max i s.t. sum [0..i) < w
--
-- findMaxIndexLT k [1, 1..1] == k - 1
--
-- >>> ones <- buildFenwickTree [1, 1, 1, 1, 1]
-- >>> findMaxIndexLT 3 ones
-- 2
-- >>> findMaxIndexLT 0 ones
-- 0
-- >>> ids <- buildFenwickTree [1, 2, 3, 4, 5]
-- >>> findMaxIndexLT 6 ids
-- 2
-- >>> findMaxIndexLT 7 ids
-- 3
-- >>> zeros <- buildFenwickTree [0, 0, 0, 0, 0]
-- >>> findMaxIndexLT 1 zeros
-- 5
findMaxIndexLT :: (PrimMonad m, U.Unbox a, Num a, Ord a)
    => FenwickTree (PrimState m) a -> a -> m Int
findMaxIndexLT (FenwickTree ft) w0
    | w0 <= 0 = return 0
    | otherwise = go w0 highestOneBit 0
  where
    n = UM.length ft
    highestOneBit = until (>n) (*2) 1 `quot` 2
    go !w !step !i
        | step == 0 = return i
        | otherwise = do
            if i + step < n
            then do
                u <- UM.unsafeRead ft (i + step)
                if u < w
                then go (w - u) (step `unsafeShiftR` 1) (i + step)
                else go w (step `unsafeShiftR` 1) i
            else go w (step `unsafeShiftR` 1) i
{-# INLINE findMaxIndexLT #-}
