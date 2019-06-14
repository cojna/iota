{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe         #-}

module Math.Modulus where

import           Data.Bits
import           Data.Int
import qualified Data.IntMap.Strict as IM

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

-- |
-- Baby-step Giant-step
-- @a^x = b (mod p)@　p is prime
-- @O(sqrt P * P)@
logMod :: Int -> Int -> Int -> Maybe Int
logMod a b p = go 0 b
  where
    !sqrtP = ceiling . sqrt $ fromIntegral p
    !g = powMod a sqrtP p
    babyStep x = a * x `rem` p
    giantStep x = g * x `rem` p

    table :: IM.IntMap Int
    !table = IM.fromList $ zip (L.iterate babyStep 1) [0..sqrtP]

    go !i !x
        | i <= sqrtP = case IM.lookup x table of
            Just j -> Just $! i + j
            Nothing -> go (i + sqrtP) $ giantStep x
        | otherwise = Nothing
