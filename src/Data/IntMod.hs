{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE Safe         #-}

module Data.IntMod where

import           Data.Int
import           Data.Ratio

#include "MachDeps.h"

#define MOD 1000000007

#if WORD_SIZE_IN_BITS == 64
newtype IntMod = Mod{unMod :: Int} deriving (Eq, Ord)
modulus :: Int
modulus = MOD
#else
newtype IntMod = Mod{unMod :: Int64} deriving (Eq, Ord)
modulus :: Int64
modulus = MOD
#endif

intMod :: (Integral a) => a -> IntMod
intMod x = Mod $ fromIntegral $ mod x MOD
{-# INLINE intMod #-}

instance Show IntMod where
    show (Mod x) = show x

instance Bounded IntMod where
    minBound = Mod 0
    maxBound = Mod $ modulus - 1

instance Enum IntMod where
    toEnum x = Mod $ mod (fromIntegral x) modulus
    fromEnum = fromIntegral . unMod

instance Real IntMod where
    toRational (Mod x) = fromIntegral x % 1

instance Integral IntMod where
    quotRem x y = (x / y, x - x / y * y)
    toInteger (Mod x) = fromIntegral x

instance Num IntMod where
    (Mod x) + (Mod y) = Mod $ (x + y) `rem` MOD
    (Mod x) - (Mod y) = Mod $ (x - y) `mod` MOD
    (Mod x) * (Mod y) = Mod $ x * y `rem` MOD
    negate (Mod x) = Mod $ negate x `mod` MOD
    abs x = x
    signum x = Mod 1
    fromInteger = intMod

instance Fractional IntMod where
    recip (Mod x) = go x MOD 1 0
      where
        go !a !b !u !v
            | b > 0 = case a `quot` b of
                q -> go b (a - (q * b)) v (u - (q * v))
            | otherwise = Mod (u `mod` MOD)
    fromRational q = intMod (numerator q) / intMod (denominator q)

