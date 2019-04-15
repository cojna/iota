{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Data.SegTree.Vector where

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

extendToPowerOfTwo :: Int -> Int
extendToPowerOfTwo x
    | w > 1 = fromIntegral
        $ unsafeShiftR (maxBound :: Word) (countLeadingZeros (w - 1)) + 1
    | otherwise = 1
  where
    w :: Word
    w = fromIntegral x

newtype SegTree m a = SegTree { unSegTree :: UM.MVector m a }

data SegTreeQuery a
    = SegUpdate !Int !a
    | SegQuery !Int !Int

runSegTree
    :: (Monoid a, U.Unbox a)
    => U.Vector a -> V.Vector (SegTreeQuery a) -> U.Vector a
runSegTree vec queries = U.create $ do
    seg <- _SEGfromVector vec
    res <- UM.replicate (V.length queries) mempty
    size <- V.foldM' (\acc -> \case
            SegUpdate k v -> do
                _SEGupdate k v seg
                return acc
            SegQuery l r -> do
                _SEGquery l r seg >>= UM.unsafeWrite res acc
                return $ acc + 1
        ) 0 queries
    return $ UM.take size res

-- | O(n)
_SEGfromVector
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => U.Vector a -> m (SegTree (PrimState m) a)
_SEGfromVector vec = do
    let n = extendToPowerOfTwo $ U.length vec
    tree <- UM.replicate (2 * n) mempty
    U.imapM_ (UM.unsafeWrite tree . (+ n)) vec
    rev (n - 1) $ \i -> do
        x <- mappend
            <$> UM.unsafeRead tree (i .<<. 1)
            <*> UM.unsafeRead tree (i .<<. 1 .|. 1)
        UM.unsafeWrite tree i x
    return $ SegTree tree
  where
    rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
    rev n = U.forM_ $ U.iterateN n (subtract 1) (n - 1)
    {-# INLINE rev #-}
    infixl 8 .<<.
    (.<<.) :: Int -> Int -> Int
    (.<<.) = unsafeShiftL
    {-# INLINE (.<<.) #-}

-- | O(log n)
_SEGupdate
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> a -> SegTree (PrimState m) a -> m ()
_SEGupdate k v segtree = do
    let tree = unSegTree segtree
    let n = UM.length tree .>>. 1
    UM.unsafeWrite tree (k + n) v
    flip fix (k + n) $ \loop !i ->
        when (i > 1) $ do
            x <- mappend
                <$> UM.unsafeRead tree i
                <*> UM.unsafeRead tree (i .^. 1)
            UM.unsafeWrite tree (i .>>. 1) x
            loop $ unsafeShiftR i 1
  where
    infixl 8 .>>.
    (.>>.) :: Int -> Int -> Int
    (.>>.) = unsafeShiftR
    {-# INLINE (.>>.) #-}
    infixl 6 .^.
    (.^.) :: Int -> Int -> Int
    (.^.) = xor
    {-# INLINE (.^.) #-}

-- | mappend [l..r) O(log n)
_SEGquery
    :: (Monoid a, U.Unbox a, PrimMonad m)
    => Int -> Int -> SegTree (PrimState m) a -> m a
_SEGquery l r segtree = do
    let tree = unSegTree segtree
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
  where
    infixl 8 .>>.
    (.>>.) :: Int -> Int -> Int
    (.>>.) = unsafeShiftR
    {-# INLINE (.>>.) #-}

