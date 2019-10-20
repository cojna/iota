{-# LANGUAGE BangPatterns #-}

module Algorithm.BinarySearch.Int where

import           Control.Exception
import           Data.Bits
import           Utils

lowerBoundInt :: Int -> Int -> (Int -> Bool) -> Int
lowerBoundInt low high p = assert (p high) $ go low high
  where
    go !low !high
        | high <= low = high
        | p mid       = go low       mid
        | otherwise   = go (mid + 1) high
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE lowerBoundInt #-}

upperBoundInt :: Int -> Int -> (Int -> Bool) -> Int
upperBoundInt low high p
    | p high = high
    | otherwise = lowerBoundInt low high (not.p) - 1
{-# INLINE upperBoundInt #-}
