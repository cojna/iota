{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Word64 where

import Data.Bits
import Data.Word
import Unsafe.Coerce

{- |
 @encode64@ / @decode64@

 +-------------------+-------------+-----------+
 | @Int@             | - 4 * 10^18 | 4 * 10^18 |
 +-------------------+-------------+-----------+
 | @(Int, Int)@      |      -10^9  |     10^9  |
 +-------------------+-------------+-----------+
 | @(Int, Int, Int)@ |      -10^6  |     10^6  |
 +-------------------+-------------+-----------+

 @encodeNonNegative64@ / @decodeNonNegative64@

 +-------------------+---+-----------+
 | @Int@             | 0 | 9 * 10^18 |
 +-------------------+---+-----------+
 | @(Int, Int)@      | 0 | 2 * 10^9  |
 +-------------------+---+-----------+
 | @(Int, Int, Int)@ | 0 | 2 * 10^6  |
 +-------------------+---+-----------+
-}
class Word64Encode a where
  encode64 :: a -> Word64
  decode64 :: Word64 -> a

  -- | for non-negative
  encodeNonNegative64 :: a -> Word64
  encodeNonNegative64 = encode64

  -- | for non-negative
  decodeNonNegative64 :: Word64 -> a
  decodeNonNegative64 = decode64

instance Word64Encode Int where
  encode64 x = unsafeCoerce $ x + 0x3fffffffffffffff
  decode64 x = unsafeCoerce x - 0x3fffffffffffffff
  encodeNonNegative64 = unsafeCoerce
  decodeNonNegative64 = unsafeCoerce

instance Word64Encode (Int, Int) where
  encode64 (x, y) =
    unsafeCoerce $
      unsafeShiftL (x + 0x3fffffff) 31 .|. (y + 0x3fffffff)
  decode64 xy = unsafeCoerce (x, y)
    where
      !x = unsafeShiftR xy 31 - 0x3fffffff
      !y = (xy .&. 0x7fffffff) - 0x3fffffff
  encodeNonNegative64 (x, y) = unsafeCoerce $ unsafeShiftL x 31 .|. y
  decodeNonNegative64 xy = unsafeCoerce (x, y)
    where
      !x = unsafeShiftR xy 31
      !y = xy .&. 0x7fffffff

instance Word64Encode (Int, Int, Int) where
  encode64 (x, y, z) =
    unsafeCoerce $
      unsafeShiftL (unsafeShiftL (x + 0xfffff) 21 .|. (y + 0xfffff)) 21 .|. (z + 0xfffff)
  decode64 xyz = unsafeCoerce (x, y, z)
    where
      !x = unsafeShiftR xyz 42 - 0xfffff
      !y = (unsafeShiftR xyz 21 .&. 0x1fffff) - 0xfffff
      !z = xyz .&. 0x1fffff - 0xfffff
  encodeNonNegative64 (x, y, z) =
    unsafeCoerce $
      unsafeShiftL (unsafeShiftL x 21 .|. y) 21 .|. z
  decodeNonNegative64 xyz = unsafeCoerce (x, y, z)
    where
      !x = unsafeShiftR xyz 42
      !y = unsafeShiftR xyz 21 .&. 0x1fffff
      !z = xyz .&. 0x1fffff
