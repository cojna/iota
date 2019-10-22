

{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables           #-}

module Data.Heap.Binary where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Coerce
import           Data.Function
import           Data.Functor.Identity
import           Data.Ord
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
--
import           Utils

data BinaryHeap (f :: * -> *) s a = BinaryHeap
    { priorityBH :: a -> f a
    , intVarsBH :: !(UM.MVector s Int)
    , internalVecBH :: !(UM.MVector s a)
    }

type MinBinaryHeap s a = BinaryHeap Identity s a
type MaxBinaryHeap s a = BinaryHeap Down s a

newBinaryHeap :: (PrimMonad m, U.Unbox a) => (a -> f a) -> Int -> m (BinaryHeap f (PrimState m) a)
newBinaryHeap prio n = BinaryHeap prio <$> UM.replicate 1 0 <*> UM.unsafeNew n

newMinBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (MinBinaryHeap (PrimState m) a)
newMinBinaryHeap n = BinaryHeap Identity <$> UM.replicate 1 0 <*> UM.unsafeNew n

newMaxBinaryHeap :: (PrimMonad m, U.Unbox a) => Int -> m (MaxBinaryHeap (PrimState m) a)
newMaxBinaryHeap n = BinaryHeap Down <$> UM.replicate 1 0 <*> UM.unsafeNew n

getBinaryHeapSize :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m Int
getBinaryHeapSize (BinaryHeap _ vars _) = UM.unsafeRead vars 0
{-# INLINE getBinaryHeapSize #-}

siftUpBy :: (PrimMonad m, U.Unbox a)
    => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftUpBy cmp k vec = do
    x <- UM.unsafeRead vec k
    flip fix k $ \loop !i ->
        if i > 0
        then do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            case cmp p x of
                GT -> do
                    UM.unsafeWrite vec i p
                    loop parent
                _ -> UM.unsafeWrite vec i x
        else UM.unsafeWrite vec 0 x
{-# INLINE siftUpBy #-}

siftDownBy :: (PrimMonad m, U.Unbox a)
    => (a -> a -> Ordering) -> Int -> UM.MVector (PrimState m) a -> m ()
siftDownBy cmp k vec = do
    x <- UM.unsafeRead vec k
    let !n = UM.length vec
    flip fix k $ \loop !i -> do
        let l = unsafeShiftL i 1 .|. 1
        let r = l + 1
        if n <= l
        then UM.unsafeWrite vec i x
        else do
            vl <- UM.unsafeRead vec l
            if r < n
            then do
                vr <- UM.unsafeRead vec r
                case cmp vr vl of
                    LT -> case cmp x vr of
                        GT -> UM.unsafeWrite vec i vr >> loop r
                        _  -> UM.unsafeWrite vec i x
                    _ -> case cmp x vl of
                        GT -> UM.unsafeWrite vec i vl >> loop l
                        _  -> UM.unsafeWrite vec i x
            else case cmp x vl of
                GT -> UM.unsafeWrite vec i vl >> loop l
                _  -> UM.unsafeWrite vec i x
{-# INLINE siftDownBy #-}

heapifyBy :: (PrimMonad m, U.Unbox a)
    => (a -> a -> Ordering) -> UM.MVector (PrimState m) a -> m ()
heapifyBy cmp vec = do
    rev (UM.length vec `quot` 2) $ \i -> do
        siftDownBy cmp i vec
{-# INLINE heapifyBy #-}

class OrdVia f a where
    compareVia :: (a -> f a) -> a -> a -> Ordering

instance (Ord a) => OrdVia Identity a where
    compareVia _ = coerce (compare :: Identity a -> Identity a -> Ordering)
    {-# INLINE compareVia #-}

instance (Ord a) => OrdVia Down a where
    compareVia _ = coerce (compare :: Down a -> Down a -> Ordering)
    {-# INLINE compareVia #-}

buildBinaryHeapVia :: (PrimMonad m, U.Unbox a, OrdVia f a)
    => (a -> f a) -> U.Vector a -> m (BinaryHeap f (PrimState m) a)
buildBinaryHeapVia ~f vec = do
    vars <- UM.replicate 1 $ U.length vec
    mvec <- U.unsafeThaw vec
    heapifyBy (compareVia f) mvec
    return $! BinaryHeap f vars mvec
{-# INLINE buildBinaryHeapVia #-}

buildMinBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a)
    => U.Vector a -> m (BinaryHeap Identity (PrimState m) a)
buildMinBinaryHeap = buildBinaryHeapVia Identity
{-# INLINE buildMinBinaryHeap #-}

buildMaxBinaryHeap :: (PrimMonad m, U.Unbox a, Ord a)
    => U.Vector a -> m (BinaryHeap Down (PrimState m) a)
buildMaxBinaryHeap = buildBinaryHeapVia Down
{-# INLINE buildMaxBinaryHeap #-}

unsafeViewBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap f (PrimState m) a -> m a
unsafeViewBH (BinaryHeap _ _ vec) = UM.unsafeRead vec 0
{-# INLINE unsafeViewBH #-}

viewBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap f (PrimState m) a -> m (Maybe a)
viewBH bh = do
    size <- getBinaryHeapSize bh
    if size > 0
    then Just <$!> unsafeViewBH bh
    else return $! Nothing
{-# INLINE viewBH #-}

insertBH :: (PrimMonad m, U.Unbox a, OrdVia f a)
    => a -> BinaryHeap f (PrimState m) a -> m ()
insertBH x bh@(BinaryHeap prio vars vec) = do
    size <- getBinaryHeapSize bh
    UM.unsafeWrite vars 0 (size + 1)
    UM.unsafeWrite vec size x
    siftUpBy (compareVia prio) size vec
{-# INLINE insertBH #-}

unsafeDeleteBH :: (PrimMonad m, U.Unbox a, OrdVia f a)
    => BinaryHeap f (PrimState m) a -> m ()
unsafeDeleteBH bh@(BinaryHeap prio vars vec) = do
    size' <- subtract 1 <$!> getBinaryHeapSize bh
    UM.unsafeWrite vars 0 size'
    UM.unsafeSwap vec 0 size'
    siftDownBy (compareVia prio) 0 (UM.unsafeTake size' vec)
{-# INLINE unsafeDeleteBH #-}

modifyTopBH :: (PrimMonad m, U.Unbox a, OrdVia f a)
    => BinaryHeap f (PrimState m) a -> (a -> a) -> m ()
modifyTopBH bh@(BinaryHeap prio _ vec) f = do
    UM.unsafeModify vec f 0
    size <- getBinaryHeapSize bh
    siftDownBy (compareVia prio) 0 (UM.unsafeTake size vec)
{-# INLINE modifyTopBH #-}

deleteFindTopBH :: (PrimMonad m, U.Unbox a, Ord a)
    => MinBinaryHeap (PrimState m) a -> m (Maybe a)
deleteFindTopBH bh@(BinaryHeap _ _ vec) = do
    size <- getBinaryHeapSize bh
    if size > 0
    then do
        !top <- unsafeViewBH bh <* unsafeDeleteBH bh
        return $ Just top
    else return Nothing
{-# INLINE deleteFindTopBH #-}

clearBH :: (PrimMonad m) => BinaryHeap f (PrimState m) a -> m ()
clearBH (BinaryHeap _ vars _) = UM.unsafeWrite vars 0 0

freezeInternalBinaryHeapBH :: (PrimMonad m, U.Unbox a)
    => BinaryHeap f (PrimState m) a -> m (U.Vector a)
freezeInternalBinaryHeapBH bh@(BinaryHeap _ _ vec) = do
    size <- getBinaryHeapSize bh
    U.unsafeFreeze (UM.unsafeTake size vec)
