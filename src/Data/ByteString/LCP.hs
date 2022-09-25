{-# LANGUAGE BangPatterns #-}

module Data.ByteString.LCP where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.ByteString.SuffixArray
import My.Prelude

newtype LCPArray = LCPArray {getLCPArray :: U.Vector Int} deriving (Show)

buildLCPArray :: B.ByteString -> SuffixArray -> LCPArray
buildLCPArray bs sa = LCPArray $
  U.create $ do
    lcp <- UM.unsafeNew (n + 1)
    UM.unsafeWrite lcp 0 0
    U.ifoldM'_
      ( \h i r -> do
          let !j = indexSA sa (r - 1)
              h' =
                fix
                  ( \loop !d ->
                      if i + d < n
                        && j + d < n
                        && B.unsafeIndex bs (i + d) == B.unsafeIndex bs (j + d)
                        then loop (d + 1)
                        else d
                  )
                  (max 0 (h - 1))
          UM.unsafeWrite lcp (r - 1) h'
          pure h'
      )
      0
      $ U.init rank
    return lcp
  where
    !n = B.length bs
    !rank = U.create $ do
      buf <- UM.unsafeNew (n + 1)
      rep (n + 1) $ \i -> do
        UM.unsafeWrite buf (indexSA sa i) i
      return buf
