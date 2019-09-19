{-# LANGUAGE BangPatterns #-}

module Data.ByteString.LCP where

import           Control.Monad
import qualified Data.ByteString             as B
import qualified Data.ByteString.Unsafe      as B
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM

import           Data.ByteString.SuffixArray
import           Utils

newtype LCPArray = LCPArray {getLCPArray :: U.Vector Int} deriving (Show)

naiveLCP :: B.ByteString -> B.ByteString -> Int
naiveLCP xs ys = go 0
   where
     !n = min (B.length xs) (B.length ys)
     go !i
       | i < n && B.unsafeIndex xs i == B.unsafeIndex ys i = go (i+1)
       | otherwise = i

buildLCPArray :: B.ByteString -> SuffixArray -> LCPArray
buildLCPArray bs sa = LCPArray $ U.create $ do
  let !n = B.length bs
      rank = U.unsafeAccumulate (flip const) (U.generate (n + 1) id)
           . U.imap (flip (,))
           $ getSuffixArray sa
  lcp <- UM.replicate (n + 1) (0 :: Int)
  let go !i !h
        | i < n = do
            let xs = B.unsafeDrop (i + h) bs
            let ys = B.unsafeDrop (getSuffixStartPos sa (U.unsafeIndex rank i - 1) + h) bs
            let h' = h + naiveLCP xs ys
            UM.unsafeWrite lcp (U.unsafeIndex rank i - 1) h'
            go (i + 1) . max 0 $ h' - 1
        | otherwise = return lcp
  go 0 0