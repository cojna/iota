{-# LANGUAGE BangPatterns, TypeApplications #-}

-- xoroshiro128**
-- http://vigna.di.unimi.it/xorshift/
module System.Random.XoRoShiRo where

import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.Primitive.ByteArray as BA
import           Data.Word
import           Unsafe.Coerce

newtype RNG s = RNG (BA.MutableByteArray s)

newRNG :: (PrimMonad m) => m (RNG (PrimState m))
newRNG = newRNGWithSeed 123456789

newRNGWithSeed :: (PrimMonad m) => Word -> m (RNG (PrimState m))
newRNGWithSeed seed = do
    let (s0, seed') = splitMix64 seed
    let (s1, _) = splitMix64 seed'
    mba <- BA.newByteArray (2 * 8)
    BA.writeByteArray @Word mba 0 s0
    BA.writeByteArray @Word mba 1 s1
    return $ RNG mba

nextInt :: (PrimMonad m) => RNG (PrimState m) -> m Int
nextInt rng = unsafeCoerce @Word @Int <$> nextWord rng

nextWord :: (PrimMonad m) => RNG (PrimState m) -> m Word
nextWord (RNG mba) = do
    !s0 <- BA.readByteArray @Word mba 0
    !s1 <- xor s0 <$> BA.readByteArray @Word mba 1
    BA.writeByteArray mba 0 $
        (unsafeShiftL s0 24 .|. unsafeShiftR s0 40)
            `xor` s1
            `xor` (unsafeShiftL s1 16)
    BA.writeByteArray mba 1 $ unsafeShiftL s1 37 .|. unsafeShiftR s1 27
    let s05 = s0 * 5
    return $! (unsafeShiftL s05 7 .|. unsafeShiftR s05 57) * 9

-- | [0.0..1.0)
nextDouble :: (PrimMonad m) => RNG (PrimState m) -> m Double
nextDouble rng = do
    t <- nextWord rng
    let x = unsafeShiftL 0x3ff 52 .|. unsafeShiftR t 12
    return $! unsafeCoerce @Word @Double x - 1.0

nextGauss :: (PrimMonad m)
    => RNG (PrimState m)
    -> Double -> Double -> m Double
nextGauss rng mu sigma = do
    x <- nextDouble rng
    y <- nextDouble rng
    let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
    return $! sigma * z + mu

splitMix64 :: Word -> (Word, Word)
splitMix64 state = case state + 0x9e3779b97f4a7c15 of
    z0 -> case (z0 `xor` unsafeShiftR z0 30) * 0xbf58476d1ce4e5b9 of
        z1 -> case (z1 `xor` unsafeShiftR z1 27) * 0x94d049bb133111eb of
            z2 -> case z2 `xor` unsafeShiftR z2 31 of
                z3 -> (z0, z3)
