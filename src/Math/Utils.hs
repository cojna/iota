{-# LANGUAGE TypeApplications #-}

module Math.Utils where

import Data.Bits
import Data.Word
import Unsafe.Coerce

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt @Double . fromIntegral

{- |
 >>> floorLog2 0
 -1023
 >>> floorLog2 1
 0
 >>> floorLog2 2
 1
 >>> floorLog2 1023
 9
 >>> floorLog2 1024
 10
-}
floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)
