{-# LANGUAGE RecordWildCards #-}

module Data.VecStack where

import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

data VecStack s a = VecStack
    { intVarsVS        :: !(UM.MVector s Int)
    , internalVecStack :: !(UM.MVector s a)
    }

_sizeVS :: Int
_sizeVS = 0
{-# INLINE _sizeVS #-}

newVecStack :: (PrimMonad m, UM.Unbox a) => Int -> m (VecStack (PrimState m) a)
newVecStack n = VecStack <$> UM.replicate 1 0 <*> UM.unsafeNew n

defaultVecStackSize :: Int
defaultVecStackSize = 1024 * 1024

popVS :: (PrimMonad m, UM.Unbox a) => VecStack (PrimState m) a -> m (Maybe a)
popVS (VecStack info s) = do
    len <- UM.unsafeRead info _sizeVS
    if len > 0
    then do
        UM.unsafeWrite info _sizeVS (len - 1)
        pure <$> UM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popVS #-}

pushVS :: (PrimMonad m, UM.Unbox a) => a -> VecStack (PrimState m) a -> m ()
pushVS x (VecStack info s) = do
    len <- UM.unsafeRead info _sizeVS
    UM.unsafeWrite s len x
    UM.unsafeWrite info _sizeVS (len + 1)
{-# INLINE pushVS #-}

pushesVS :: (PrimMonad m, UM.Unbox a)
    => U.Vector a -> VecStack (PrimState m) a -> m ()
pushesVS vec (VecStack info s) = do
    len <- UM.unsafeRead info _sizeVS
    UM.unsafeWrite info _sizeVS (len + U.length vec)
    U.unsafeCopy (UM.unsafeSlice len (U.length vec) s) vec
{-# INLINE pushesVS #-}

freezeVecStack :: (PrimMonad m, U.Unbox a) => VecStack (PrimState m) a -> m (U.Vector a)
freezeVecStack (VecStack info s) = do
    l <- UM.unsafeRead info _sizeVS
    U.unsafeFreeze $ UM.take l s
{-# INLINE freezeVecStack #-}
