{-# LANGUAGE BangPatterns #-}

module Utils.Random where

import           Data.Bits
import           Data.Word

xorshift128 :: [Word32]
xorshift128 = go 123456789 362436069 521288629 88675123
  where
    go !x !y !z !w = neww : go y z w neww
      where
        !t = x `xor` unsafeShiftL x 11
        !neww = (w `xor` unsafeShiftR w 19) `xor` (t `xor` unsafeShiftR t 8)
