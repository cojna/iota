{-# LANGUAGE BangPatterns #-}

module Math.Modulus.Log where

import qualified Data.IntMap.Strict as IM

import Math.Modulus (powMod)

{- |
 Baby-step Giant-step

 @a^x = b (mod p)@ p is prime

 /O(sqrt P * log P)/

 >>> logMod 3 27 998244353
 Just 3
 >>> logMod 3 123456789 998244353
 Just 772453214
 >>> logMod 1 2 1000000007
 Nothing
-}
logMod :: Int -> Int -> Int -> Maybe Int
logMod a b p = go 0 b
  where
    !sqrtP = ceiling . sqrt $ fromIntegral p
    !g = powMod a (- sqrtP) p
    babyStep x = a * x `rem` p
    giantStep x = g * x `rem` p

    table :: IM.IntMap Int
    !table = IM.fromList $ zip (iterate babyStep 1) [0 .. sqrtP]

    go !i !x
      | i < sqrtP = case IM.lookup x table of
        Just j -> Just $! i * sqrtP + j
        Nothing -> go (i + 1) $ giantStep x
      | otherwise = Nothing
