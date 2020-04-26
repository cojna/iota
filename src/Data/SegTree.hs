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
import           Data.Bits.Utils
import           Utils                       (rev)

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | w > 1 = fromIntegral
        $ unsafeShiftR (maxBound :: Word) (countLeadingZeros (w - 1)) + 1
    | otherwise = 1
  where
    w :: Word
    w = fromIntegral x

newtype SegTree m a = SegTree { getSegTree :: UM.MVector m a }

-- | O(n)
buildSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
buildSegTree vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.unsafeCopy (UM.unsafeSlice n (U.length vec) tree) vec
    rev (n - 1) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (i .<<. 1)
            <*> UM.unsafeRead tree (i .<<. 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree

-- | O(log n)
writeSegTree
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> a -> m ()
writeSegTree segtree k v = do
    let tree = getSegTree segtree
    let n = UM.length tree .>>. 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i .^. 1)
            UM.unsafeWrite tree (i .>>. 1) x
            loop $ unsafeShiftR i 1

-- | mappend [l..r)
--
-- O(log n)
mappendFromTo
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => SegTree (PrimState m) a -> Int -> Int -> m a
mappendFromTo segtree l r = do
    let tree = getSegTree segtree
    let n = UM.length tree .>>. 1
    let stepL l
            | l .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree l
            | otherwise = return

        stepR r
            | r .&. 1 == 1 = \acc ->
                mappend acc <$> UM.unsafeRead tree (r - 1)
            | otherwise = return

        go l r k
            | l < r = go ((l + l .&. 1) .>>. 1) ((r - r .&. 1) .>>. 1)
                $ stepL l >=> (stepR r >=> k)
            | otherwise = k
    go (n + l) (n + r) return mempty
