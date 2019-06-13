{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE Unsafe       #-}

module Data.IntMod where

import           Data.Bits
import           Data.Coerce
import           Data.Ratio
import           GHC.Exts

#define MOD 1000000007

modulus :: (Num a) => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
    r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}

(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
    r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}

(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
{-# INLINE (*%) #-}

-- |
-- >>> 1 /% 0
-- 0
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
            q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}

(^%) :: Int -> Int -> Int
x ^% n
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
        | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
        | m == 1 = acc *% y
        | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype IntMod = IntMod{unIntMod :: Int} deriving (Eq, Ord)

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
    maxBound = IntMod $ modulus - 1

instance Enum IntMod where
    toEnum = intMod
    fromEnum = coerce

instance Real IntMod where
    toRational = coerce (toRational :: Int -> Rational)

instance Integral IntMod where
    quotRem x y = (x / y, x - x / y * y)
    toInteger = coerce (toInteger :: Int -> Integer)

instance Num IntMod where
    (+) = coerce (+%)
    (-) = coerce (-%)
    (*) = coerce (*%)
    abs = id
    signum = const (IntMod 1)
    fromInteger x = (coerce :: Int -> IntMod) . fromInteger $ mod x modulus

instance Fractional IntMod where
    (/) = coerce (/%)
    fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
