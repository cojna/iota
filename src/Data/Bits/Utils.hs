module Data.Bits.Utils where

import Data.Bits

infixl 8 .<<., .>>.
infixl 6 .^.

(.<<.) :: (Bits i) => i -> Int -> i
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: (Bits i) => i -> Int -> i
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.^.) :: (Bits i) => i -> i -> i
(.^.) = xor
{-# INLINE (.^.) #-}
