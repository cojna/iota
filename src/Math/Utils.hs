module Math.Utils where

import Data.Bits

{- |
>>> floorSqrt 0
0
>>> floorSqrt 1
1
>>> floorSqrt 2
1
>>> floorSqrt 4
2
>>> floorSqrt (12345 * 12345)
12345
>>> floorSqrt (2^52 + 2^27)
67108864
>>> floorSqrt maxBound
3037000499
-}
floorSqrt :: Int -> Int
floorSqrt n
  | n <= 1 = n
  | otherwise =
      let !k = 32 - unsafeShiftR (countLeadingZeros (n - 1)) 1
          !x0 = unsafeShiftL 1 k
          !x1 = unsafeShiftR (x0 + unsafeShiftR n k) 1
       in go x0 x1
  where
    go !x !x'
      | x' < x = go x' (unsafeShiftR (x' + quot n x') 1)
      | otherwise = x

{- |
BSR (Bit Scan Reverse)

>>> floorLog2 0
-1
>>> floorLog2 1
0
>>> floorLog2 2
1
>>> floorLog2 1023
9
>>> floorLog2 1024
10
>>> floorLog2 maxBound
62
-}
floorLog2 :: Int -> Int
floorLog2 x = 63 - countLeadingZeros x
{-# INLINE floorLog2 #-}
