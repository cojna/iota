{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- xoroshiro128**
-- http://vigna.di.unimi.it/xorshift/
module System.Random.XoRoShiRo where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.ByteArray
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word
import Unsafe.Coerce

import My.Prelude (rev)

newtype RNG s = RNG (MutableByteArray s)

defaultSeed :: Word
defaultSeed = 123456789

newRNG :: (PrimMonad m) => m (RNG (PrimState m))
newRNG = newRNGWithSeed defaultSeed

newRNGWithSeed :: (PrimMonad m) => Word -> m (RNG (PrimState m))
newRNGWithSeed seed = do
  let (s0, seed') = splitMix64 seed
  let (s1, _) = splitMix64 seed'
  mba <- newByteArray (2 * 8)
  writeByteArray @Word mba 0 s0
  writeByteArray @Word mba 1 s1
  return $ RNG mba

withRNG :: (forall s. RNG s -> ST s a) -> a
withRNG = withRNGWithSeed defaultSeed

withRNGWithSeed :: Word -> (forall s. RNG s -> ST s a) -> a
withRNGWithSeed seed f = runST $ newRNGWithSeed seed >>= f

nextInt :: (PrimMonad m) => RNG (PrimState m) -> m Int
nextInt rng = unsafeCoerce @Word @Int <$> nextWord rng

nextWord :: (PrimMonad m) => RNG (PrimState m) -> m Word
nextWord (RNG mba) = do
  !s0 <- readByteArray @Word mba 0
  !s1 <- xor s0 <$> readByteArray @Word mba 1
  writeByteArray mba 0 $
    (unsafeShiftL s0 24 .|. unsafeShiftR s0 40)
      `xor` s1
      `xor` (unsafeShiftL s1 16)
  writeByteArray mba 1 $ unsafeShiftL s1 37 .|. unsafeShiftR s1 27
  let s05 = s0 * 5
  return $! (unsafeShiftL s05 7 .|. unsafeShiftR s05 57) * 9

-- | [0.0..1.0)
nextDouble :: (PrimMonad m) => RNG (PrimState m) -> m Double
nextDouble rng = do
  t <- nextWord rng
  let x = unsafeShiftL 0x3ff 52 .|. unsafeShiftR t 12
  return $! unsafeCoerce @Word @Double x - 1.0

nextGauss ::
  (PrimMonad m) =>
  RNG (PrimState m) ->
  Double ->
  Double ->
  m Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

shuffle :: (G.Vector v a) => v a -> v a
shuffle v = withRNG $ \rng -> do
  mv <- G.thaw v
  shuffleM rng mv
  G.unsafeFreeze mv

shuffleM ::
  (PrimMonad m, GM.MVector mv a) =>
  RNG (PrimState m) ->
  mv (PrimState m) a ->
  m ()
shuffleM rng mv = do
  rev (GM.length mv) $ \i -> do
    j <- nextWord rng
    GM.unsafeSwap mv i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

splitMix64 :: Word -> (Word, Word)
splitMix64 state = case state + 0x9e3779b97f4a7c15 of
  z0 -> case (z0 `xor` unsafeShiftR z0 30) * 0xbf58476d1ce4e5b9 of
    z1 -> case (z1 `xor` unsafeShiftR z1 27) * 0x94d049bb133111eb of
      z2 -> case z2 `xor` unsafeShiftR z2 31 of
        z3 -> (z0, z3)
