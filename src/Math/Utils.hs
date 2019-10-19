module Math.Utils where

import           Data.Bits
import           Data.Word
import           Unsafe.Coerce

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

floorLog2 :: Int -> Int
floorLog2 x = fromIntegral $ unsafeShiftR y 52 - 1023
  where
    y :: Word64
    y = unsafeCoerce (fromIntegral x :: Double)
