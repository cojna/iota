module Data.VecStack where

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as UM

data VecStack m a = VecStack
    { stackInfo :: !(UM.MVector m Int)
    , stackData :: !(UM.MVector m a)
    }

newStackM :: (PrimMonad m, UM.Unbox a) => m (VecStack (PrimState m) a)
newStackM = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew (1024 * 1024)

popM :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
popM (VecStack info s) = do
    len <- UM.unsafeRead info 0
    if len > 0
    then do
        UM.unsafeWrite info 0 (len - 1)
        pure <$> UM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popM #-}

pushM :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
pushM x (VecStack info s) = do
    len <- UM.unsafeRead info 0
    UM.unsafeWrite s len x
    UM.unsafeWrite info 0 (len + 1)
{-# INLINE pushM #-}
