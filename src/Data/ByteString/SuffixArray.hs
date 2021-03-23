{-# LANGUAGE BangPatterns #-}

module Data.ByteString.SuffixArray where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

newtype SuffixArray = SuffixArray {getSuffixArray :: U.Vector Int} deriving (Show)

getSuffixStartPos :: SuffixArray -> Int -> Int
getSuffixStartPos = U.unsafeIndex . getSuffixArray
{-# INLINE getSuffixStartPos #-}

getSuffix :: SuffixArray -> Int -> B.ByteString -> B.ByteString
getSuffix = (B.unsafeDrop .) . getSuffixStartPos
{-# INLINE getSuffix #-}

-- | n < 2 ^ 20 (= 1,048,576)
buildSuffixArray :: B.ByteString -> SuffixArray
buildSuffixArray bs =
  SuffixArray
    . fst
    . F.foldl step (sa0, rank0)
    . takeWhile (<= n)
    $ map (shiftL 1) [0 ..]
  where
    !n = B.length bs :: Int
    !sa0 = U.generate (n + 1) id
    !rank0 = U.generate n (fromIntegral . B.unsafeIndex bs) `U.snoc` 0
    step (!sa, !rank) k = (sa', rank')
      where
        encode sai =
          let !x = rank U.! sai
              !y
                | sai + k <= n = rank U.! (sai + k)
                | otherwise = 0
           in unsafeShiftL x 40 .|. unsafeShiftL y 20 .|. sai
        maskSA x = x .&. 0xfffff
        maskRank x = unsafeShiftR x 20

        sorted = radixSort64 $ U.map encode sa
        !sa' = U.map maskSA sorted
        !rank' = U.create $ do
          mv <- UM.unsafeNew (n + 1)
          UM.write mv (sa' U.! 0) 1

          U.forM_ (U.zip sorted $ U.tail sorted) $ \(prev, cur) -> do
            x <- UM.unsafeRead mv (maskSA prev)
            UM.unsafeWrite mv (maskSA cur) $
              x + fromEnum (maskRank prev < maskRank cur)

          return mv

radixSort64 :: U.Vector Int -> U.Vector Int
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
