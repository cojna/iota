{-# LANGUAGE BangPatterns, MagicHash #-}

module Data.Bits.Utils where

import           Data.Bits
import           Data.Word
import           GHC.Exts
import           Unsafe.Coerce

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

-- |
-- >>> bitReverse 0x000f00f60000f6f6
-- 8029636662487216128
-- >>> 0x6f6f00006f00f000
-- 8029636662487216128
bitReverse :: Int -> Int
bitReverse
    = unsafeCoerce
    . step 32 0xffffffff00000000 0x00000000ffffffff
    . step 16 0xffff0000ffff0000 0x0000ffff0000ffff
    . step 08 0xff00ff00ff00ff00 0x00ff00ff00ff00ff
    . step 04 0xf0f0f0f0f0f0f0f0 0x0f0f0f0f0f0f0f0f
    . step 02 0xcccccccccccccccc 0x3333333333333333
    . step 01 0xaaaaaaaaaaaaaaaa 0x5555555555555555
    . unsafeCoerce
  where
    step :: Int -> Word64 -> Word64 -> Word64 -> Word64
    step i ml mr = \ !x -> unsafeShiftR (x .&. ml) i .|. unsafeShiftL (x .&. mr) i
    {-# INLINE step #-}
