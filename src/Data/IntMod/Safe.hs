{-# LANGUAGE BangPatterns, CPP, Safe #-}

module Data.IntMod.Safe where

import           Data.Bits
import           Data.Int
import           Data.Ratio

#define MOD 1000000007

modulus :: (Num a) => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int64 -> Int64 -> Int64
x +% y = case x + y of
    r | r < MOD -> r
      | otherwise -> r - MOD
{-# INLINE (+%) #-}

(-%) :: Int64 -> Int64 -> Int64
x -% y = case x - y of
    r | r < 0 -> r + MOD
      | otherwise -> r
{-# INLINE (-%) #-}

(*%) :: Int64 -> Int64 -> Int64
x *% y = x * y `rem` MOD
{-# INLINE (*%) #-}

-- |
-- >>> 1 /% 0
-- 0
(/%) :: Int64 -> Int64 -> Int64
x /% y = go y MOD 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - q * b) v (u - q * v)
        | otherwise = x * (u + MOD) `rem` MOD
{-# INLINE (/%) #-}

(^%) :: Int64 -> Int -> Int64
x ^% n
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
        | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
        | m == 1 = acc *% y
        | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype IntMod = IntMod{getIntMod :: Int64} deriving (Eq, Ord)

intMod :: (Integral a) => a -> IntMod
intMod x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE intMod #-}

intModValidate :: IntMod -> Bool
intModValidate (IntMod x) = 0 <= x && x < MOD
{-# INLINE intModValidate #-}

instance Show IntMod where
    show (IntMod x) = show x

instance Bounded IntMod where
    minBound = IntMod 0
    maxBound = IntMod (MOD - 1)

instance Enum IntMod where
    toEnum = intMod
    fromEnum = fromIntegral

instance Real IntMod where
    toRational (IntMod x) = toRational x

instance Integral IntMod where
    quotRem x y = (x / y, x - x / y * y)
    toInteger (IntMod x) = toInteger x

instance Num IntMod where
    (IntMod x) + (IntMod y) = IntMod (x +% y)
    (IntMod x) - (IntMod y) = IntMod (x -% y)
    (IntMod x) * (IntMod y) = IntMod (x *% y)
    abs = id
    signum = const (IntMod 1)
    fromInteger x = IntMod . fromInteger $ mod x modulus

instance Fractional IntMod where
    (IntMod x) / (IntMod y) = IntMod (x /% y)
    fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
