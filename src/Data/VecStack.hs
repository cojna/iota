module Data.VecStack where

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as UM

data VecStack s a = VecStack
    { stackInfo :: !(UM.MVector s Int)
    , stackData :: !(UM.MVector s a)
    }

newVecStack :: (PrimMonad m, UM.Unbox a) => Int -> m (VecStack (PrimState m) a)
newVecStack n = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew n

defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024

pop :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
pop (VecStack info s) = do
    len <- UM.unsafeRead info 0
    if len > 0
    then do
        UM.unsafeWrite info 0 (len - 1)
        pure <$> UM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE pop #-}

push :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
push x (VecStack info s) = do
    len <- UM.unsafeRead info 0
    UM.unsafeWrite s len x
    UM.unsafeWrite info 0 (len + 1)
{-# INLINE push #-}
