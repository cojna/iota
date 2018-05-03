{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe         #-}

module Math.Modulus where

import           Data.Bits
import           Data.Int

powMod :: (Integral a, Integral b, Bits b) => a -> b -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (recipMod x m) (-n)
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `rem` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

recipMod :: (Integral a) => a -> a -> a
recipMod x m = go x m 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = u `mod` m
{-# INLINE recipMod #-}
