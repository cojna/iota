{-# LANGUAGE BangPatterns, CPP #-}

module Data.Heap.BinaryHeap.Max where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
--
import           Utils

data BinaryHeap s a = BinaryHeap (MutVar s Int) (UM.MVector s a)

newBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (BinaryHeap (PrimState m) a)
newBinaryHeap n = BinaryHeap <$> newMutVar 0 <*> UM.new n

getBinaryHeapSize :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap ref _) = readMutVar ref
{-# INLINE getBinaryHeapSize #-}

siftUp :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftUp k vec = do
    x <- UM.unsafeRead vec k
    flip fix k $ \loop !i ->
        if i > 0
        then do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p >= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
        else UM.unsafeWrite vec 0 x
{-# INLINE siftUp #-}

siftDown :: (PrimMonad m, U.Unbox a, Ord a) => Int -> UM.MVector (PrimState m) a -> m ()
siftDown k vec = do
    x <- UM.unsafeRead vec k
    let n = UM.length vec
    flip fix k $ \loop !i -> do
        let !l = unsafeShiftL i 1 .|. 1
        if n <= l
        then UM.unsafeWrite vec i x
        else do
            let !r = l + 1
            childL <- UM.unsafeRead vec l
            childR <- UM.unsafeRead vec r
            if r < n && childR > childL
            then if x >= childR
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childR
                     loop r
            else if x >= childL
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childL
                     loop l
{-# INLINE siftDown #-}

heapify :: (PrimMonad m, U.Unbox a, Ord a) => UM.MVector (PrimState m) a -> m ()
heapify vec = do
    rev (UM.length vec `quot` 2) $ \i -> do
        siftDown i vec
{-# INLINE heapify #-}

buildBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a)
    => U.Vector a -> m (BinaryHeap (PrimState m) a)
buildBinaryHeap vec = do
    ref <- newMutVar $ U.length vec
    mvec <- U.unsafeThaw vec
    heapify mvec
    return $! BinaryHeap ref mvec
{-# INLINE buildBinaryHeap #-}

unsafeMaxViewBH :: (PrimMonad m, U.Unbox a) => BinaryHeap (PrimState m) a -> m a
unsafeMaxViewBH (BinaryHeap _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeMaxViewBH #-}

maxViewBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
maxViewBH bh = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMaxViewBH bh
    else return $! Nothing
{-# INLINE maxViewBH #-}

insertMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> BinaryHeap (PrimState m) a -> m ()
insertMaxBH x bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (+1)
    UM.unsafeWrite vec size x
    siftUp size vec
{-# INLINE insertMaxBH #-}

unsafeDeleteMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m ()
unsafeDeleteMaxBH bh@(BinaryHeap info vec) = do
    size <- getBinaryHeapSize bh
    modifyMutVar' info (subtract 1)
    UM.unsafeSwap vec 0 (size - 1)
    siftDown 0 (UM.unsafeTake (size - 1) vec)
{-# INLINE unsafeDeleteMaxBH #-}

modifyMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> (a -> a) -> m ()
modifyMaxBH bh@(BinaryHeap _ vec) f = do
    UM.unsafeModify vec f 0
    size <- getBinaryHeapSize bh
    siftDown 0 (UM.unsafeTake size vec)
{-# INLINE modifyMaxBH #-}

deleteFindMaxBH :: (PrimMonad m, U.Unbox a, Ord a)
    => BinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindMaxBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeMaxViewBH bh <* unsafeDeleteMaxBH bh
    else return $! Nothing
{-# INLINE deleteFindMaxBH #-}

clearBH :: (PrimMonad m) => BinaryHeap (PrimState m) a -> m ()
clearBH (BinaryHeap info _) = writeMutVar info 0

freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ vec) = do
    size <- getBinaryHeapSize bh
    U.unsafeFreeze (UM.unsafeTake size vec)
