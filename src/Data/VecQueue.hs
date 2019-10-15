module Data.VecQueue where

import           Control.Monad.Primitive
import qualified Data.Vector.Primitive       as PV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data VecQueue s a = VecQueue
    { queueInfo :: !(UM.MVector s Int)
    , queueData :: !(UM.MVector s a)
    }

getEnqueueCount :: (PrimMonad m) => VecQueue (PrimState m) a -> m Int
getEnqueueCount (VecQueue info _) = UM.unsafeRead info 1
{-# INLINE getEnqueueCount #-}

getDequeueCount :: (PrimMonad m) => VecQueue (PrimState m) a -> m Int
getDequeueCount (VecQueue info _) = UM.unsafeRead info 0
{-# INLINE getDequeueCount #-}

newVecQueue :: (PrimMonad m, UM.Unbox a) => Int -> m (VecQueue (PrimState m) a)
newVecQueue n = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n

defaultVecQueueSize :: Int
defaultVecQueueSize = 1024 * 1024

freezeQueueData
    :: (PrimMonad m, UM.Unbox a)
    => VecQueue (PrimState m) a -> m (U.Vector a)
freezeQueueData vq@(VecQueue info q) = do
    len <- getEnqueueCount vq
    U.unsafeFreeze $ UM.take len q

lengthQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthQ vq
    = (-) <$> getEnqueueCount vq <*> getDequeueCount vq

dequeue :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeue vq@(VecQueue info q) = do
    h <- getDequeueCount vq
    t <- getEnqueueCount vq
    if h < t
    then do
        UM.unsafeWrite info 0 (h + 1)
        pure <$> UM.unsafeRead q h
    else return Nothing
{-# INLINE dequeue #-}

dequeueAll :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
dequeueAll vq@(VecQueue info q) = do
    h <- getDequeueCount vq
    t <- getEnqueueCount vq
    U.unsafeFreeze $ UM.unsafeSlice h (t - h) q
{-# INLINE dequeueAll #-}

enqueue :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueue x vq@(VecQueue info q) = do
    t <- getEnqueueCount vq
    UM.unsafeWrite q t x
    UM.unsafeWrite info 1 (t + 1)
{-# INLINE enqueue #-}
