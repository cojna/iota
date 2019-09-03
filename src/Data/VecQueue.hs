module Data.VecQueue where

import           Control.Monad.Primitive
import qualified Data.Vector.Primitive       as PV
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data VecQueue s a = VecQueue
    { queueInfo :: !(UM.MVector s Int)
    , queueData :: !(UM.MVector s a)
    }

withCapacityQ
    :: (PrimMonad m, UM.Unbox a)
    => Int -> m (VecQueue (PrimState m) a)
withCapacityQ n
    = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew n

newQueue :: (PrimMonad m, UM.Unbox a) => m (VecQueue (PrimState m) a)
newQueue = withCapacityQ (1024 * 1024)

freezeQueueData
    :: (PrimMonad m, UM.Unbox a)
    => VecQueue (PrimState m) a -> m (U.Vector a)
freezeQueueData (VecQueue info q) = do
    len <- UM.unsafeRead info 1
    U.unsafeFreeze $ UM.take len q

lengthQ :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m Int
lengthQ (VecQueue info q)
    = (-) <$> UM.unsafeRead info 1 <*> UM.unsafeRead info 0

dequeue :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeue (VecQueue info q) = do
    h <- UM.unsafeRead info 0
    t <- UM.unsafeRead info 1
    if h < t
    then do
        UM.unsafeWrite info 0 (h + 1)
        pure <$> UM.unsafeRead q h
    else return Nothing
{-# INLINE dequeue #-}

dequeueAll :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (U.Vector a)
dequeueAll (VecQueue info q) = do
    h <- UM.unsafeRead info 0
    t <- UM.unsafeRead info 1
    U.unsafeFreeze $ UM.unsafeSlice h (t - h) q
{-# INLINE dequeueAll #-}

enqueue :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueue x (VecQueue info q) = do
    t <- UM.unsafeRead info 1
    UM.unsafeWrite q t x
    UM.unsafeWrite info 1 (t + 1)
{-# INLINE enqueue #-}
