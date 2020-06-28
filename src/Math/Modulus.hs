{-# LANGUAGE BangPatterns, Safe #-}

module Math.Modulus where

import           Data.Bits
import           Data.Int
import qualified Data.IntMap.Strict as IM

-- |
-- >>> powMod 2 0 1000000007
-- 1
-- >>> powMod 0 0 1000000007
-- 1
-- >>> powMod 2 1000000005 1000000007
-- 500000004
-- >>> powMod 2 (-1) 1000000007
-- 500000004
-- >>> powMod 123456789 998244353 998244353
-- 123456789
-- >>> powMod (-2) 2 1000000007
-- 4
-- >>> powMod (-2) 3 1000000007
-- 999999999
powMod :: (Integral a) => a -> Int -> a -> a
powMod x n m
    | n > 0 = go 1 x n
    | n == 0 = 1
    | otherwise = go 1 (recipMod x m) (-n)
  where
    go !acc !y !i
        | i .&. 1 == 0 = go acc (y * y `rem` m) (unsafeShiftR i 1)
        | i == 1 = acc * y `mod` m
        | otherwise = go (acc * y `rem` m) (y * y `rem` m) (unsafeShiftR (i - 1) 1)
{-# INLINE powMod #-}

-- |
-- >>> recipMod 2 1000000007
-- 500000004
-- >>> recipMod 10 1000000007
-- 700000005
recipMod :: (Integral a) => a -> a -> a
recipMod x m = go x m 1 0
  where
    go !a !b !u !v
        | b > 0 = case a `quot` b of
            q -> go b (a - (q * b)) v (u - (q * v))
        | otherwise = u `mod` m
{-# INLINE recipMod #-}

-- |
-- Baby-step Giant-step
--
-- @a^x = b (mod p)@ p is prime
--
-- /O(sqrt P * log P)/
--
-- >>> logMod 3 27 998244353
-- Just 3
-- >>> logMod 3 123456789 998244353
-- Just 772453214
-- >>> logMod 1 2 1000000007
-- Nothing
logMod :: Int -> Int -> Int -> Maybe Int
logMod a b p = go 0 b
  where
    !sqrtP = ceiling . sqrt $ fromIntegral p
    !g = powMod a (-sqrtP) p
    babyStep x = a * x `rem` p
    giantStep x = g * x `rem` p

    table :: IM.IntMap Int
    !table = IM.fromList $ zip (iterate babyStep 1) [0..sqrtP]

    go !i !x
        | i < sqrtP = case IM.lookup x table of
            Just j  -> Just $! i * sqrtP + j
            Nothing -> go (i + 1) $ giantStep x
        | otherwise = Nothing
