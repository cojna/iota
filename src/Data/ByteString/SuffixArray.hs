{-# LANGUAGE BangPatterns #-}

module Data.ByteString.SuffixArray where

import           Control.Monad
import qualified Data.ByteString             as B
import qualified Data.ByteString.Unsafe      as B
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Utils

newtype SuffixArray = SuffixArray {getSuffixArray :: U.Vector Int} deriving (Show)

getSuffixStartPos :: SuffixArray -> Int -> Int
getSuffixStartPos = U.unsafeIndex . getSuffixArray
{-# INLINE getSuffixStartPos #-}

getSuffix :: SuffixArray -> Int -> B.ByteString -> B.ByteString
getSuffix  = (B.unsafeDrop.) . getSuffixStartPos
{-# INLINE getSuffix #-}

buildSuffixArray :: B.ByteString -> SuffixArray
buildSuffixArray bs = SuffixArray $ U.create $ do
  let !n = B.length bs :: Int
      !countMax = max 0xff (n+1)

  sa    <- UM.replicate (n + 1) (0 :: Int)
  rank  <- UM.replicate (n + 1) (0 :: Int)
  tmp   <- UM.replicate (n + 1) (0 :: Int)
  count <- UM.replicate (countMax + 1) (0 :: Int)

  rep n $ \i-> do
    UM.unsafeWrite sa i i
    UM.unsafeWrite rank i . fromIntegral $ B.unsafeIndex bs i
  UM.unsafeWrite sa n n
  UM.unsafeWrite rank n 1

  forM_ (takeWhile (<=n) $ iterate (*2) 1) $ \k-> do
    -- sort sa
    rep (countMax+1) $ \i-> do
      UM.unsafeWrite count i 0
    rep (n+1) $ \i-> do
      sai <- UM.unsafeRead sa i
      if sai+k<=n then do
        rankik <- UM.unsafeRead rank (sai+k)
        UM.unsafeModify count (+1) rankik
      else UM.unsafeModify count (+1) 0
    rep countMax $ \i-> do
      cnti <- UM.unsafeRead count i
      UM.unsafeModify count (+cnti) (i+1)
    rev (n+1) $ \i-> do
      sai <- UM.unsafeRead sa i
      rankik <- if sai + k <= n then do
                   UM.unsafeRead rank (sai+k)
                else return 0
      j <- subtract 1 <$> UM.unsafeRead count rankik
      UM.unsafeWrite count rankik j
      UM.unsafeRead sa i>>=UM.unsafeWrite tmp j

    rep (countMax+1) $ \i-> do
      UM.unsafeWrite count i 0
    rep (n+1) $ \i-> do
      sai <- UM.unsafeRead tmp i
      ranki <- UM.unsafeRead rank sai
      UM.unsafeModify count (+1) ranki
    rep countMax $ \i-> do
      cnti <- UM.unsafeRead count i
      UM.unsafeModify count (+cnti) (i+1)
    rev (n+1) $ \i-> do
      sai <- UM.unsafeRead tmp i
      ranki <- UM.unsafeRead rank sai
      j <- subtract 1 <$> UM.unsafeRead count ranki
      UM.unsafeWrite count ranki j
      UM.unsafeWrite sa j sai

    -- update rank
    sa0 <- UM.unsafeRead sa 0
    UM.unsafeWrite tmp sa0 1
    rep n $ \i-> do
      sai    <- UM.unsafeRead sa i
      sai1   <- UM.unsafeRead sa (i+1)
      ranki  <- UM.unsafeRead rank sai
      ranki1 <- UM.unsafeRead rank sai1
      if ranki == ranki1 then do
           rankik  <- if sai  + k > n then return (-1)
                      else UM.unsafeRead rank (sai+k)
           ranki1k <- if sai1 + k > n then return (-1)
                      else UM.unsafeRead rank (sai1+k)
           if rankik == ranki1k then do
             UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1
           else do
             UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1 . (+1)
      else do
        UM.unsafeRead tmp sai >>= UM.unsafeWrite tmp sai1 . (+1)

    rep (n+1) $ \i-> do
      UM.unsafeRead tmp i >>= UM.unsafeWrite rank i

  return sa
