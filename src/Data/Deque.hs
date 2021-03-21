module Data.Deque where

import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

data Deque s a = Deque
  { dequeVars :: !(UM.MVector s Int)
  , getDeque :: !(UM.MVector s a)
  }

_dequeFrontPos :: Int
_dequeFrontPos = 0

_dequeBackPos :: Int
_dequeBackPos = 1

newDeque :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newDeque n = Deque <$> UM.replicate 2 n <*> UM.unsafeNew (2 * n)

defaultDequeSize :: Int
defaultDequeSize = 1024 * 1024

type Queue s a = Deque s a

newQueue :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newQueue = newBuffer

clearQueue :: (U.Unbox a, PrimMonad m) => Deque (PrimState m) a -> m ()
clearQueue = clearBuffer

type Stack s a = Deque s a

newStack :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newStack = newBuffer

clearStack :: (U.Unbox a, PrimMonad m) => Deque (PrimState m) a -> m ()
clearStack = clearBuffer

type Buffer s a = Deque s a

newBuffer :: (PrimMonad m, U.Unbox a) => Int -> m (Deque (PrimState m) a)
newBuffer n = Deque <$> UM.replicate 2 0 <*> UM.unsafeNew n

lengthBuffer :: (PrimMonad m) => Deque (PrimState m) a -> m Int
lengthBuffer = lengthDeque

clearBuffer :: (PrimMonad m) => Deque (PrimState m) a -> m ()
clearBuffer (Deque vars _) = do
  UM.unsafeWrite vars _dequeFrontPos 0
  UM.unsafeWrite vars _dequeBackPos 0

freezeBuffer ::
  (PrimMonad m, U.Unbox a) =>
  Deque (PrimState m) a ->
  m (U.Vector a)
freezeBuffer (Deque info v) = do
  b <- UM.unsafeRead info _dequeBackPos
  U.freeze $ UM.unsafeTake b v

unsafeFreezeBuffer ::
  (PrimMonad m, U.Unbox a) =>
  Deque (PrimState m) a ->
  m (U.Vector a)
unsafeFreezeBuffer (Deque info v) = do
  b <- UM.unsafeRead info _dequeBackPos
  U.unsafeFreeze $ UM.unsafeTake b v

lengthDeque :: (PrimMonad m) => Deque (PrimState m) a -> m Int
lengthDeque (Deque info _) =
  (-) <$> UM.unsafeRead info _dequeBackPos
    <*> UM.unsafeRead info _dequeFrontPos
{-# INLINE lengthDeque #-}

popFront :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m (Maybe a)
popFront (Deque info v) = do
  f <- UM.unsafeRead info _dequeFrontPos
  b <- UM.unsafeRead info _dequeBackPos
  if f < b
    then do
      UM.unsafeWrite info _dequeFrontPos (f + 1)
      pure <$> UM.unsafeRead v f
    else return Nothing
{-# INLINE popFront #-}

popBack :: (PrimMonad m, U.Unbox a) => Deque (PrimState m) a -> m (Maybe a)
popBack (Deque info v) = do
  f <- UM.unsafeRead info _dequeFrontPos
  b <- UM.unsafeRead info _dequeBackPos
  if f < b
    then do
      UM.unsafeWrite info _dequeBackPos (b - 1)
      pure <$> UM.unsafeRead v (b - 1)
    else return Nothing
{-# INLINE popBack #-}

pushFront :: (PrimMonad m, U.Unbox a) => a -> Deque (PrimState m) a -> m ()
pushFront x (Deque info v) = do
  f <- UM.unsafeRead info _dequeFrontPos
  UM.unsafeWrite v (f - 1) x
  UM.unsafeWrite info _dequeFrontPos (f - 1)
{-# INLINE pushFront #-}

pushBack :: (PrimMonad m, U.Unbox a) => a -> Deque (PrimState m) a -> m ()
pushBack x (Deque info v) = do
  b <- UM.unsafeRead info _dequeBackPos
  UM.unsafeWrite v b x
  UM.unsafeWrite info _dequeBackPos (b + 1)
{-# INLINE pushBack #-}

pushFronts ::
  (PrimMonad m, U.Unbox a) =>
  U.Vector a ->
  Deque (PrimState m) a ->
  m ()
pushFronts vec (Deque info v) = do
  let n = U.length vec
  f <- UM.unsafeRead info _dequeFrontPos
  UM.unsafeWrite info _dequeFrontPos (f - n)
  U.unsafeCopy (UM.unsafeSlice (f - n) n v) vec
{-# INLINE pushFronts #-}

pushBacks ::
  (PrimMonad m, U.Unbox a) =>
  U.Vector a ->
  Deque (PrimState m) a ->
  m ()
pushBacks vec (Deque info v) = do
  let n = U.length vec
  b <- UM.unsafeRead info _dequeBackPos
  UM.unsafeWrite info _dequeBackPos (b + n)
  U.unsafeCopy (UM.unsafeSlice b n v) vec
{-# INLINE pushBacks #-}

clearDeque :: (U.Unbox a, PrimMonad m) => Deque (PrimState m) a -> m ()
clearDeque (Deque info v) = do
  let o = UM.length v `quot` 2
  UM.unsafeWrite info _dequeFrontPos o
  UM.unsafeWrite info _dequeBackPos o

freezeDeque ::
  (PrimMonad m, U.Unbox a) =>
  Deque (PrimState m) a ->
  m (U.Vector a)
freezeDeque (Deque info v) = do
  f <- UM.unsafeRead info _dequeFrontPos
  b <- UM.unsafeRead info _dequeBackPos
  U.freeze $ UM.unsafeSlice f (b - f) v
