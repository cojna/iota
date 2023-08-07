{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.ByteString.RollingHash where

import Data.Bits
import qualified Data.ByteString as B
import GHC.Exts
import GHC.Word

-- | @rollingHash == rollingHashWith 2047@
rollingHash :: B.ByteString -> Int
rollingHash = B.foldl' step 0
  where
    m# = 0x1fffffffffffffff#
    step (I# acc#) (W8# w8#) =
      case uncheckedIShiftRL# acc# 50#
        +# andI# (uncheckedIShiftL# acc# 11#) m#
        -# acc#
        +# word2Int# (word8ToWord# w8#) of
        qr# -> case qr# <# 0# of
          b# -> I# (qr# -# b# +# uncheckedIShiftL# b# 61#)

-- | @base@ should be a primitive root of (@2^61-1@)
rollingHashWith :: Int -> B.ByteString -> Int
rollingHashWith base = B.foldl' (\acc w -> acc *% base +% fromIntegral w) 0

infixr 8 ^%
infixl 7 *%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
  xy# -> case xy# >=# 0x1fffffffffffffff# of
    b# -> I# (xy# +# b# -# uncheckedIShiftL# b# 61#)
{-# INLINE (+%) #-}

(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
  xy# -> case xy# <# 0# of
    b# -> I# (xy# -# b# +# uncheckedIShiftL# b# 61#)
{-# INLINE (-%) #-}

(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = case timesWord2# (int2Word# x#) (int2Word# y#) of
  (# hi#, lo# #) ->
    case uncheckedIShiftL# (word2Int# hi#) 3#
      +# uncheckedIShiftRL# (word2Int# lo#) 61#
      +# andI# (word2Int# lo#) 0x1fffffffffffffff# of
      qr# -> case qr# ># 0x1fffffffffffffff# of
        b# -> I# (qr# +# b# -# uncheckedIShiftL# b# 61#)
{-# INLINE (*%) #-}

(^%) :: Int -> Int -> Int
x ^% n
  | n > 0 = go 1 x n
  | n == 0 = 1
  | otherwise = go 1 (x ^% 0x1ffffffffffffffd) (-n)
  where
    go !acc !y !m
      | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
      | m == 1 = acc *% y
      | otherwise = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

{- |
>>> take 10 $ filter isPrimitiveRootRH [2..]
[37,43,55,69,74,86,110,123,133,138]
-}
isPrimitiveRootRH :: Int -> Bool
isPrimitiveRootRH g = all ok [2, 3, 5, 7, 11, 13, 31, 41, 61, 151, 331, 1321]
  where
    ok p = g ^% quot 0x1ffffffffffffffe p /= 1
