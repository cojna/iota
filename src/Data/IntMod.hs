{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.IntMod where

import Data.Bits
import Data.Coerce
import Data.Primitive
import Data.Ratio
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import GHC.Exts

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
(I# x#) *% (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#
        | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
        | otherwise -> I# (word2Int# v#)
  where
    m# = int2Word# MOD#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
{-# INLINE (*%) #-}

{- |
>>> 1 /% 0
0
-}
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

newtype IntMod = IntMod {getIntMod :: Int}
  deriving newtype (Eq, Ord, Read, Show, Real, Prim)

intMod :: (Integral a) => a -> IntMod
intMod x = fromIntegral $ mod (toInteger x) MOD
{-# INLINE intMod #-}

intModValidate :: IntMod -> Bool
intModValidate (IntMod x) = 0 <= x && x < MOD
{-# INLINE intModValidate #-}

instance Bounded IntMod where
  minBound = IntMod 0
  maxBound = IntMod $ modulus - 1

instance Enum IntMod where
  toEnum = intMod
  fromEnum = coerce

instance Integral IntMod where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Int)

instance Num IntMod where
  (+) = coerce (+%)
  (-) = coerce (-%)
  (*) = coerce (*%)
  abs = id
  signum = const (IntMod 1)
  fromInteger x = coerce @Int @IntMod . fromInteger $ mod x modulus

instance Fractional IntMod where
  (/) = coerce (/%)
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

newtype instance U.MVector s IntMod = MV_IntMod (U.MVector s Int)
newtype instance U.Vector IntMod = V_IntMod (U.Vector Int)
deriving newtype instance GM.MVector U.MVector IntMod
deriving newtype instance G.Vector U.Vector IntMod
instance U.Unbox IntMod
