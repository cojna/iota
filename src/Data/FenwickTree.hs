{-# LANGUAGE BangPatterns #-}

module Data.FenwickTree where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word

newtype FenwickTree s a = FenwickTree {getFenwickTree :: UM.MVector s a}

newFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> m (FenwickTree (PrimState m) a)
newFenwickTree n = FenwickTree <$> UM.replicate (n + 1) 0

buildFenwickTree :: (PrimMonad m, U.Unbox a, Num a)
    => U.Vector a -> m (FenwickTree (PrimState m) a)
buildFenwickTree vec = do
    let n = U.length vec
    ft <- UM.replicate (n + 1) 0
    U.unsafeCopy (UM.tail ft) vec
    U.forM_ (U.generate n (+1)) $ \i -> do
        let j = i + (i .&. (-i))
        when (j <= n) $ do
            fti <- UM.unsafeRead ft i
            UM.unsafeModify ft (+fti) j
    return $ FenwickTree ft

getFreezeFenwickTree :: (PrimMonad m, U.Unbox a)
    => FenwickTree (PrimState m) a -> m (U.Vector a)
getFreezeFenwickTree (FenwickTree ft) = do
    U.freeze ft


-- | sum [0..k)
sumTo :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> FenwickTree (PrimState m) a -> m a
sumTo k (FenwickTree ft) = go 0 k
  where
    go !acc !i
        | i > 0 = do
            xi <- UM.unsafeRead ft i
            go (acc + xi) (i - (i .&. (-i)))
        | otherwise = return acc
{-# INLINE sumTo #-}

-- | sum [l..r)
sumFromTo :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> Int -> FenwickTree (PrimState m) a -> m a
sumFromTo l r ft = (-) <$> sumTo r ft <*> sumTo l ft
{-# INLINE sumFromTo #-}

addAt :: (PrimMonad m, U.Unbox a, Num a)
    => Int -> a -> FenwickTree (PrimState m) a -> m ()
addAt k v (FenwickTree ft) = flip fix (k + 1) $ \loop !i -> do
    when (i < n) $ do
        UM.unsafeModify ft (+v) i
        loop $ i + (i .&. (-i))
  where
    n = UM.length ft
{-# INLINE addAt #-}

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
    => a -> FenwickTree (PrimState m) a -> m Int
findMaxIndexLT w0 (FenwickTree ft)
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
