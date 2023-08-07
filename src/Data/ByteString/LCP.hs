module Data.ByteString.LCP where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Function
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.ByteString.SuffixArray
import My.Prelude

newtype LCPArray = LCPArray {getLCPArray :: U.Vector Int} deriving (Show)

{- | /O(n)/

* lcp[i] = lcp(sa[i], sa[i+1])
* lcp(s[l],s[r]) = minimum[lcp[sa[l]],lcp[sa[l]+1]..lcp[sa[r]-1]]

>>> :set -XOverloadedStrings
>>> bs = "abracadabra"
>>> buildLCPArray bs $ buildSuffixArray bs
LCPArray {getLCPArray = [0,1,4,1,1,0,3,0,0,0,2]}
-}
buildLCPArray :: B.ByteString -> SuffixArray Int32 -> LCPArray
buildLCPArray bs sa = LCPArray $
  U.create $ do
    lcp <- UM.unsafeNew n
    UM.unsafeWrite lcp 0 0
    U.ifoldM'_
      ( \h i r -> do
          let !j = fromIntegral $ indexSA sa (r - 1)
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
        UM.unsafeWrite buf (fromIntegral $ indexSA sa i) i
      return buf
