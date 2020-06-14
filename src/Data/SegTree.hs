{-# LANGUAGE BangPatterns, LambdaCase #-}

module Data.SegTree where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Function
import           Data.Monoid
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Data.Word
--
import           Utils                       (rev, unsafeShiftRL)

-- |
-- >>> extendToPowerOfTwo 0
-- 1
extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
    | otherwise = 1

newtype SegTree s a = SegTree { getSegTree :: UM.MVector s a }

newSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> m (SegTree (PrimState m) a)
newSegTree n = SegTree <$> UM.replicate (2 * extendToPowerOfTwo n) mempty

-- | /O(n)/
buildSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length vec) tree) vec
    rev (n - 1) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (unsafeShiftL i 1)
            <*> UM.unsafeRead tree (unsafeShiftL i 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree

-- | /O(log n)/
writeSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i `xor` 1)
            UM.unsafeWrite tree (unsafeShiftRL i 1) x
            loop $ unsafeShiftR i 1

-- | mappend [l..r)
--
-- /O(log n)/
mappendFromTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = unsafeShiftRL (UM.length tree) 1
    let stepL l
            | l .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return

        stepR r
            | r .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return

        go l r k
            | l < r = go (unsafeShiftRL (l + l .&. 1) 1) (unsafeShiftRL (r - r .&. 1) 1)
                $ stepL l >=> (stepR r >=> k)
            | otherwise = k
    go (n + l) (n + r) return mempty

-- | mappend [0..k)
--
-- /O(log n)/
mappendTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> m a
mappendTo segtree = mappendFromTo segtree 0
{-# INLINE mappendTo #-}
