module Data.VecQueue where

import           Control.Monad.Primitive
import qualified Data.Vector.Primitive       as PV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data VecQueue s a = VecQueue
    { intVarsVQ        :: !(UM.MVector s Int)
    , internalVecQueue :: !(UM.MVector s a)
    }

_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}

_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}

newVecQueue :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
newVecQueue n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n

defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024

lengthVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthVQ (VecQueue info _)
    = (-) <$> UM.unsafeRead info _enqueueCount
        <*> UM.unsafeRead info _dequeueCount
{-# INLINE lengthVQ #-}

dequeueVQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeueVQ (VecQueue info q) = do
    f <- UM.unsafeRead info _dequeueCount
    r <- UM.unsafeRead info _enqueueCount
    if f < r
    then do
        UM.unsafeWrite info _dequeueCount (f + 1)
        pure <$> UM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueVQ #-}

enqueueVQ :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueueVQ x (VecQueue info q) = do
    r <- UM.unsafeRead info _enqueueCount
    UM.unsafeWrite q r x
    UM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueVQ #-}

enqueuesVQ :: (PrimMonad m, UM.Unbox a)
    => U.Vector a -> VecQueue (PrimState m) a -> m ()
enqueuesVQ vec (VecQueue info q) = do
    r <- UM.unsafeRead info _enqueueCount
    UM.unsafeWrite info _enqueueCount (r + U.length vec)
    U.unsafeCopy (UM.unsafeSlice r (U.length vec) q) vec
{-# INLINE enqueuesVQ #-}

clearVQ :: (UM.Unbox a, PrimMonad m) => VecQueue (PrimState m) a -> m ()
clearVQ (VecQueue info _) = do
    UM.unsafeWrite info _dequeueCount 0
    UM.unsafeWrite info _enqueueCount 0

freezeVecQueue
    :: (PrimMonad m, UM.Unbox a)
    => VecQueue (PrimState m) a -> m (U.Vector a)
freezeVecQueue (VecQueue info q) = do
    f <- UM.unsafeRead info _dequeueCount
    r <- UM.unsafeRead info _enqueueCount
    U.unsafeFreeze $ UM.unsafeSlice f (r - f) q
