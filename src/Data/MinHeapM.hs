{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.MinHeapM where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data MinHeapM m a = MinHeapM (MutVar m Int) (UM.MVector m a)

_HMnewHeap :: (PrimMonad m, U.Unbox a) => Int -> m (MinHeapM (PrimState m) a)
_HMnewHeap limitSize = MinHeapM `liftM` newMutVar 0 `ap` UM.new limitSize

_HMgetHeapSize :: (PrimMonad m) => MinHeapM (PrimState m) a -> m Int
_HMgetHeapSize (MinHeapM ref _) = readMutVar ref
{-# INLINE _HMgetHeapSize #-}

_HMinsertM :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> MinHeapM (PrimState m) a -> m ()
_HMinsertM x heap@(MinHeapM ref vec) = do
    size <- _HMgetHeapSize heap
    modifyMutVar' ref (+1)
    flip fix size $ \loop !i ->
        if i == 0
        then UM.unsafeWrite vec 0 x
        else do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p <= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
{-# INLINE _HMinsertM #-}

_HMunsafeDeleteMinM :: (PrimMonad m, U.Unbox a, Ord a)
                 => MinHeapM (PrimState m) a -> m ()
_HMunsafeDeleteMinM (MinHeapM ref vec) = do
    modifyMutVar' ref (subtract 1)
    size <- readMutVar ref
    x <- UM.unsafeRead vec size
    flip fix 0 $ \loop !i -> do
        let !l = unsafeShiftL i 1 .|. 1
        if size <= l
        then UM.unsafeWrite vec i x
        else do
            let !r = l + 1
            childL <- UM.unsafeRead vec l
            childR <- UM.unsafeRead vec r
            if r < size && childR < childL
            then if x <= childR
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childR
                     loop r
            else if x <= childL
                 then UM.unsafeWrite vec i x
                 else do
                     UM.unsafeWrite vec i childL
                     loop l
{-# INLINE _HMunsafeDeleteMinM #-}

_HMunsafeMinViewM :: (PrimMonad m, U.Unbox a) => MinHeapM (PrimState m) a -> m a
_HMunsafeMinViewM (MinHeapM _ vec) = UM.unsafeRead vec 0
{-# INLINE _HMunsafeMinViewM #-}

_HMminViewM :: (PrimMonad m, U.Unbox a) => MinHeapM (PrimState m) a -> m (Maybe a)
_HMminViewM heap = do
    size <- _HMgetHeapSize heap
    if size > 0
    then Just `liftM` _HMunsafeMinViewM heap
    else return Nothing
{-# INLINE _HMminViewM #-}

_HMdeleteFindMinM :: (PrimMonad m, U.Unbox a, Ord a)
    => MinHeapM (PrimState m) a -> m (Maybe a)
_HMdeleteFindMinM heap = do
    size <- _HMgetHeapSize heap
    if size > 0
    then liftM2 ((Just.).const) (_HMunsafeMinViewM heap) (_HMunsafeDeleteMinM heap)
    else return Nothing
{-# INLINE _HMdeleteFindMinM #-}

_HMclearMinHeapM :: (PrimMonad m) => MinHeapM (PrimState m) a -> m ()
_HMclearMinHeapM (MinHeapM ref _) = writeMutVar ref 0
{-# INLINE _HMclearMinHeapM #-}
