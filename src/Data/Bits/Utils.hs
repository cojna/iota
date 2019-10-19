{-# LANGUAGE MagicHash #-}

module Data.Bits.Utils where

import           Data.Bits
import           GHC.Exts

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: (Bits i) => i -> Int -> i
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: (Bits i) => i -> Int -> i
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(I# x#) .>>>. (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}

(.^.) :: (Bits i) => i -> i -> i
(.^.) = xor
{-# INLINE (.^.) #-}
