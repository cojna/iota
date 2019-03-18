{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE Unsafe       #-}

module  Data.IntMod.Operator where

import           Data.Bits
import           GHC.Exts

#define MOD 1000000007

modulus :: Int
modulus = MOD

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

type IntMod = Int

intMod :: Int -> IntMod
intMod x = mod x MOD

intModValidate :: Int -> Bool
intModValidate x = 0 <= x && x < MOD

(+%) :: IntMod -> IntMod -> IntMod
(I# x#) +% (I# y#) = case x# +# y# of
    r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}

(-%) :: IntMod -> IntMod -> IntMod
(I# x#) -% (I# y#) = case x# -# y# of
    r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}

(*%) :: IntMod -> IntMod -> IntMod
(I# x#) *% (I# y#) = I# ((x# *# y#) `remInt#` MOD#)
{-# INLINE (*%) #-}

(/%) :: IntMod -> IntMod -> IntMod
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
        | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
            q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
        | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}

(^%) :: IntMod -> Int -> IntMod
x ^% n
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
        | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
        | m == 1 = acc *% y
        | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)
