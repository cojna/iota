{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Data.MaxHeapM where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Function
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data MaxHeapM m a = MaxHeapM (MutVar m Int) (UM.MVector m a)

_HHMnewHeap :: (PrimMonad m, U.Unbox a) => Int -> m (MaxHeapM (PrimState m) a)
_HHMnewHeap limitSize = MaxHeapM `liftM` newMutVar 0 `ap` UM.new limitSize

_HHMgetHeapSize :: (PrimMonad m) => MaxHeapM (PrimState m) a -> m Int
_HHMgetHeapSize (MaxHeapM ref _) = readMutVar ref
{-# INLINE _HHMgetHeapSize #-}

_HHMinsertM :: (PrimMonad m, U.Unbox a, Ord a)
    => a -> MaxHeapM (PrimState m) a -> m ()
_HHMinsertM x heap@(MaxHeapM ref vec) = do
    size <- _HHMgetHeapSize heap
    modifyMutVar' ref (+1)
    flip fix size $ \loop !i ->
        if i == 0
        then UM.unsafeWrite vec 0 x
        else do
            let !parent = (i - 1) `unsafeShiftR` 1
            p <- UM.unsafeRead vec parent
            if p >= x
            then UM.unsafeWrite vec i x
            else do
                UM.unsafeWrite vec i p
                loop parent
{-# INLINE _HHMinsertM #-}

_HHMunsafeDeleteMaxM :: (PrimMonad m, U.Unbox a, Ord a)
                 => MaxHeapM (PrimState m) a -> m ()
_HHMunsafeDeleteMaxM (MaxHeapM ref vec) = do
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
            if r < size && childR > childL
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
{-# INLINE _HHMunsafeDeleteMaxM #-}

_HHMunsafeMaxViewM :: (PrimMonad m, U.Unbox a) => MaxHeapM (PrimState m) a -> m a
_HHMunsafeMaxViewM (MaxHeapM _ vec) = UM.unsafeRead vec 0
{-# INLINE _HHMunsafeMaxViewM #-}

_HHMMaxViewM :: (PrimMonad m, U.Unbox a) => MaxHeapM (PrimState m) a -> m (Maybe a)
_HHMMaxViewM heap = do
    size <- _HHMgetHeapSize heap
    if size > 0
    then Just `liftM` _HHMunsafeMaxViewM heap
    else return Nothing
{-# INLINE _HHMMaxViewM #-}

_HHMdeleteFindMaxM :: (PrimMonad m, U.Unbox a, Ord a)
    => MaxHeapM (PrimState m) a -> m (Maybe a)
_HHMdeleteFindMaxM heap = do
    size <- _HHMgetHeapSize heap
    if size > 0
    then liftM2 ((Just.).const) (_HHMunsafeMaxViewM heap) (_HHMunsafeDeleteMaxM heap)
    else return Nothing
{-# INLINE _HHMdeleteFindMaxM #-}

_HHMclearMaxHeapM :: (PrimMonad m) => MaxHeapM (PrimState m) a -> m ()
_HHMclearMaxHeapM (MaxHeapM ref _) = writeMutVar ref 0
{-# INLINE _HHMclearMaxHeapM #-}
