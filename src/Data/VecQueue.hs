module Data.VecQueue where

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as UM

data VecQueue m a = VecQueue
    { queueInfo :: !(UM.MVector m Int)
    , queueData :: !(UM.MVector m a)
    }

newQueueM :: (PrimMonad m, UM.Unbox a) => m (VecQueue (PrimState m) a)
newQueueM = VecQueue <$> UM.replicate 2 0 <*> UM.unsafeNew (1024 * 1024)

dequeueM :: (PrimMonad m, UM.Unbox a) => VecQueue (PrimState m) a -> m (Maybe a)
dequeueM (VecQueue info q) = do
    h <- UM.unsafeRead info 0
    t <- UM.unsafeRead info 1
    if h < t
    then do
        UM.unsafeWrite info 0 (h + 1)
        pure <$> UM.unsafeRead q h
    else return Nothing
{-# INLINE dequeueM #-}

enqueueM :: (PrimMonad m, UM.Unbox a) => a -> VecQueue (PrimState m) a -> m ()
enqueueM x (VecQueue info q) = do
    t <- UM.unsafeRead info 1
    UM.unsafeWrite q t x
    UM.unsafeWrite info 1 (t + 1)
{-# INLINE enqueueM #-}
