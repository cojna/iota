module Data.Vector.Sort.Radix where

import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Unsafe.Coerce

--
import Data.Word64

radixSortInt :: U.Vector Int -> U.Vector Int
radixSortInt = unsafeCoerce . radixSort64 . unsafeCoerce

radixSort32 :: U.Vector Word32 -> U.Vector Word32
radixSort32 v0 = F.foldl' step v0 [0, 16]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
      pref <-
        U.unsafeThaw
          . U.prescanl' (+) 0
          . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
          $ U.map (flip (,) 1 . mask k) v
      res <- UM.unsafeNew $ U.length v
      U.forM_ v $ \x -> do
        let !masked = mask k x
        i <- UM.unsafeRead pref masked
        UM.unsafeWrite pref masked $ i + 1
        UM.unsafeWrite res i x
      return res
{-# INLINE radixSort32 #-}

radixSort64 :: U.Vector Word64 -> U.Vector Word64
radixSort64 v0 = F.foldl' step v0 [0, 16, 32, 48]
  where
    mask k x = fromIntegral $ unsafeShiftR x k .&. 0xffff
    step v k = U.create $ do
      pref <-
        U.unsafeThaw
          . U.prescanl' (+) 0
          . U.unsafeAccumulate (+) (U.replicate 0x10000 0)
          $ U.map (flip (,) 1 . mask k) v
      res <- UM.unsafeNew $ U.length v
      U.forM_ v $ \x -> do
        let !masked = mask k x
        i <- UM.unsafeRead pref masked
        UM.unsafeWrite pref masked $ i + 1
        UM.unsafeWrite res i x
      return res
{-# INLINE radixSort64 #-}

radixSort ::
  (U.Unbox a, Word64Encode a) =>
  U.Vector a ->
  U.Vector a
radixSort = U.map decode64 . radixSort64 . U.map encode64
{-# INLINE radixSort #-}

radixSortNonNegative ::
  (U.Unbox a, Word64Encode a) =>
  U.Vector a ->
  U.Vector a
radixSortNonNegative =
  U.map decodeNonNegative64
    . radixSort64
    . U.map encodeNonNegative64
{-# INLINE radixSortNonNegative #-}
