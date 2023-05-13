{-# LANGUAGE TypeApplications #-}

module Math.Utils where

import Data.Bits

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt @Double . fromIntegral

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
